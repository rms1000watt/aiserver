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
;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************

;;**EXPORTKEY**:Genebase
(defineStructure Genebase:
;; *************************************************************************************
;; name:     Genebase
;; 
;; summary:  Create the structure which holds the standard Genebase.
;;           Genebases are used to hold genome objects of a general
;;           nature. Each Genebase object stores the geneology for,
;;           and acts as the control structure for a genetic algorithm.
;;           The Genebase stores the individual genomes in the current
;;           generation, records the best genome in each past generation, 
;;           and controls the rate of reproduction, mutation, and cross
;;           over.
;;            
;; fields:   name:                The name of the Genebase. The default name is:
;;                                "Genebase1".
;;           generation:          The current generation count.
;;           maxGenerations:      The maximum number of generations.
;;           populationSize:      The count of genomes in each generation.
;;           survival:            The survival probability.
;;           crossOver:           The genetic cross over probability.
;;           mutation:            The genetic mutation probability.
;;           birthProc:           The birth procedure which must manage the initial 
;;                                random creation of a genome from scratch. (i.e
;;                                this procedure maps #void into a random genome.
;;           crossOverProc:       The genetic cross over procedure which maps two
;;                                genomes into a random "child" genome.
;;           mutationProc:        The genetic mutation procedure which maps a single
;;                                genome into a random "mutant" genome.
;;           fitnessProc:         The fitness procedure which maps each genome into
;;                                an integer fitness measure between 0 and 10000
;;                                where (0 is poor and 10000 is perfect). The
;;                                fitnessProc determines which genomes die and which
;;                                genomes survive into the next generation.
;;                                an integer fitness measure between 0 and 10000 (higher is better).
;;           genomeToKeyProc:     The genome to key procedure which maps each genome
;;                                into a unique identifing symbol (its key).
;;           adults:              The Dictionary of genomes in the current generation.
;;                                The Dictionary consists of unique genome keys
;;                                and spairs as values. Each spair contains two
;;                                elements as follows:
;;                                  [0] Holds an individual genome object.
;;                                  [1] The integer fitness of the genome object.
;;                                Note: The length is equal to populationSize.
;;           children:            The Dictionary of genomes in the next generation.
;;                                The Dictionary consists of unique genome keys
;;                                and spairs as values. Each spair contains two
;;                                elements as follows:
;;                                  [0] Holds an individual genome object.
;;                                  [1] The integer fitness of the genome object.
;;                                Note: The length is equal to populationSize.
;;           geneology:           The Dictionary of all genomes ever evaluated in
;;                                any generation. The Dictionary consists of unique 
;;                                genome keys and spairs as values. Each spair contains
;;                                two elements as follows:
;;                                  [0] Holds an individual genome object.
;;                                  [1] The integer fitness of the genome object.
;;                                Note: The length is equal to populationSize.
;;           history:             The row matrix recording the history of each generation.
;;                                The row matrix consists of an object vector of row  
;;                                vectors. Each row vector contains elements as follows:
;;                                  [0] The total fitness for the generation.
;;                                  [1] The worst fitness for the generation.
;;                                  [2] The average fitness for the generation.
;;                                  [3] The best fitness for the generation.
;;                                  [4] The key for the best genome of the generation.
;;                                  [5] The best genome of the generation.
;;                                  [6] The integer fitness of the best genome object.
;;                                Note: The row count is equal to maxGenerations.
;; *************************************************************************************
                            name: 
                            generation: 
                            maxGenerations: 
                            populationSize: 
                            survival: 
                            crossOver: 
                            mutation: 
                            birthProc: 
                            crossOverProc: 
                            mutationProc: 
                            fitnessProc: 
                            genomeToKeyProc: 
                            adults: 
                            children:
                            geneology:
                            history:) ;; end of Genebase



















;;**EXPORTKEY**:blackboardLambda
(defun blackboardLambda()
;; *******************************************************************
;; summary:  Uses the Workbench IO functions to open a workbench
;;           window and display the specified knowledge object (name)
;;           from the _Blackboard in the specified workbench window.
;; Args:     none
;; Return:   true
;; *******************************************************************
    pvars:(;; Methods list 
           getKeys                  ;; Return a vector of the current keys stored in the Blackboard.
           clearLogFile             ;; Clear all record strings from the Blackboard log file.
           readLogFile              ;; Read the entire log file from the Blackboard log file.
           writeLogFile             ;; Write a record string to end of the Blackboard log file.
           ) ;; end of persistent variables
    vars:(name s keys selection x y i (n 0))
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> getKeys #void) (goto Continue:))
    ;; Return a vector of the current keys stored in the Blackboard.
    (defun getKeys(startName)
        vars:(i v startIndex (j 0))
        (beginTransaction _Blackboard)
        (if (<> startName #void)
            (setq startIndex _Blackboard[position: startName]) 
            (setq startIndex 0))
        (if (isBoolean startIndex) (setq startIndex 0))
        (setq v (new Vector: (length _Blackboard)))
        (loop for i from startIndex until (length _Blackboard) do
            (setq v[j] _Blackboard[i 0])
            (setq j (add1 j))
            (if (and (<> startName #void)
                     (<> (left _Blackboard[i 0] (length startName)) startName))
                (begin
                   (abortTransaction _Blackboard)
                   (resize v j)
                   (return v)))
            ) ;; end loop
        (abortTransaction _Blackboard)
        (resize v j)
        v)
    ;; Clear all record strings from the Blackboard log file.
    (defun clearLogFile() _Blackboard.logFile #void)
    ;; Read the entire log file from the Blackboard log file.
    (defun readLogFile() _Blackboard.logFile)
    ;; Write a record string to end of the Blackboard log file.
    (defun writeLogFile(newRecord)
        vars:(record)
        (setq record _Blackboard.logFile)
        (if (= record #void) (setq record ""))
        (setq record (append record newRecord))
        (setq _Blackboard.logFile record)
        true) ;; end of writeLogFile
    ;; Make sure the blackboard is properly assigned.
    Continue::
    (if (<> (type _Blackboard) ObjectRepository:)
        (error "blackboardLambda" "The _Blackboard is not assigned properly."))
    ;; Do we wish to perform an automated search? If so, we use the
    ;; blackboard to store the results of back testing for many
    ;; different stock screening strategies. 
    (setq selection (select 
                       "Blackboard Lambda" 
                       "Please select a command." 
                       #("Browse Blackboard Selection Strategies" 
                         "Browse Blackboard Exclusion Strategies" 
                         "Browse Blackboard Measurement Strategies" 
                         "Erase from Blackboard"
                         ) ;; end of select choices
                       0))
    (if (isBoolean selection)
        (return false))
    ;; Manage the Browse screenLambdas command.
    (if (= selection 0)
        (begin
           (setq keys (getKeys screenLambda:))
           (if (<= (length keys) 0)
               (begin
                  (screenLoadStrategies)
                  (setq keys #("screenLambda"))
                  )) ;; end if
           (setq n (select  "BlackboardLambda" "Please select an object to browse." keys 0))
           (if (isBoolean n)
               (return false)
               (setq name keys[n]))
           ;; Retrieve object from blackboard.
           (setq s _Blackboard[name])
           (if (= s #void)
               (setq s "no data available"))
           ;; Open window and display object in it.
           (displayWorkbenchWindow name s)
           (return true)
        )) ;; end browse screeLambdas
    ;; Manage the Browse excludLambdas command.
    (if (= selection 1)
        (begin
           ;; Retrieve object from blackboard.
           (setq s _Blackboard[excludLambda:])
           (if (= s #void)
               (setq s (excludLoadStrategies)))
           ;; Open window and display object in it.
           (displayWorkbenchWindow name s)
           (return true)
        )) ;; end browse excludLambdas
    ;; Manage the Browse excludLambdas command.
    (if (= selection 2)
        (begin
           ;; Retrieve object from blackboard.
           (setq s _Blackboard[measureLambda:])
           (if (= s #void)
               (setq s (measureLoadStrategies)))
           ;; Open window and display object in it.
           (displayWorkbenchWindow name s)
           (return true)
        )) ;; end browse measureLambdas
    ;; Manage the Erase command.
       (if (= selection 3)
           (begin
              (setq keys (getKeys #void))
              (setq n (select  "BlackboardLambda"  "Please select an object to erase." keys 0))
              (if (isBoolean n)
                  (return false)
                  (setq name keys[n]))
           ;; Erase source from blackboard.
           (setq _Blackboard[name] #void)
           (return true)
        )) ;; end erase
    true)
















;;**EXPORTKEY**:compareTextFiles
(defun compareTextFiles(fone ftwo) 
;; *******************************************************************
;; summary:		Compare the contents of the specified files.
;; args:		fone:      The path and file name of the first file.
;;				ftwo:      The path and file name of the second file.
;; return:		result:    TRUE if the files are equal. 
;; *******************************************************************
   vars:(i j fileOne  fileTwo)
   (setq fileOne (readTextFile fone))
   (setq fileTwo (readTextFile ftwo))
   (= fileOne fileTwo))  


























;;**EXPORTKEY**:copyFile
(defun copyFile(fileOne fileTwo) 
;; *******************************************************************
;; summary:  Copy the entire contents of file one to file two.
;; Args :    fileOne    The name of the file to be copied. 
;;           fileTwo    The name of the file to receive the copy. 
;; Return:   The number of bytes copied.
;; *******************************************************************
   vars:(fileOneID           ;; The file id for file one.
         fileTwoID           ;; The file id for file two.
         dataBlock           ;; The block of data to be copied.
         (fileType 0)        ;; The file type for standard files.
         (byteCount 0)       ;; The number of bytes to be copied.
         byteIndex           ;; The number of bytes already copied.
         (byteLength 100000) ;; The number of bytes to be read.
         ) ;; end of temporary variables
   ;; Open file one for reading and file two for writing.
   (setq fileOneID (fileOpen fileOne 0 fileType))
   (setq fileTwoID (fileOpen fileTwo 1 fileType))
   ;; Find the length of file one and reset file pointer.
   (setq byteCount (fileSeek fileOneID 0 2))
   (fileSeek fileOneID 0 1)
   ;; Read and write blocks until there is no more data
   (loop for byteIndex from 0 until byteCount by 100000 do
      ;; Make sure we read only to the end of the file.
      (if (> (+ byteIndex byteLength) byteCount)
          (setq byteLength (- byteCount byteIndex))
          ) ;; end if
      ;; Copy a block from file one to file two.
      (setq dataBlock (fileRead fileOneID byteLength))
      (fileWrite fileTwoID dataBlock)
      ) ;; end of loop
   ;; Close both files.
   (fileClose fileOneID 1)
   (fileClose fileTwoID 1)
   byteCount) ;; end of copyFile
















;;**EXPORTKEY**:copyVector
(defun copyVector(x)
;; *******************************************************************
;; name:     copyVector
;; 
;; summary:  Returns a shallow copy of the input vector.
;; Parms:    x:       A vector.
;; Return:   v:       A shallow copy of the vector.
;; *******************************************************************
    vars:(i m v)
    (setq m (length x))
    (setq v (new Vector: m))
    (loop for i from 0 until m do
		(setq v[i] x[i]))
    v)

























;;**EXPORTKEY**:dataMineLib
(defun dataMineLib(dataMineExtents)
;; *******************************************************************
;; Summary:  Data Mining Librarian Lambda which manages a multiple table 
;;           data mine and controls the data mine schema. The data mine
;;           schema is similar to a relational schema with enhancements  
;;           for Many Dimensional data models, Primary tables, table 
;;           Views, Blackboard knowledge sharing areas, Goal, Factory, 
;;           Filter, and Fix up Lambdas.
;;
;;           The data mine contains two areas. The Primary area, and the
;;           Blackboard area. Both these area may contain tables. The
;;           __usingForCreate function destermines in which area new
;;           tables are created.
;;
;; Args:    dataMineExtents   The Structure of data mine file extents.
;; Return:  true
;;
;;          The initialization argument must contain a Structure of data mine 
;;          file extents. The data mine extents are divided into the primary 
;;          data area and the blackboard area. There may be multiple extents 
;;          allocated for the primary data and the blackboard. The attributes 
;;          of the data mine extent structure is as follows.
;; 
;;        Attributes:  
;;              primary:        The Vector of extent path and file names for primary tables.
;;              blackboard:     The Vector of extent path and file names for the Blackboard.
;;
;; Example:
;;	
;;   This example initializes a data mine with two primary data extents 
;;   and three blackboard extents. Each extent must specify the complete 
;;   path and file name. Extents may be placed on different physical devices 
;;   as database management strategy requires. Each extent supports up to 
;;   2 gigabytes of storage space.
;;
;;         (define _dataMineExtents  #{	primary: 	#("primary.db"  "primary.d01")
;;                          		blackboard: 	#("blacktst.db"  "blacktst.d01"  "blacktst.d02")
;;                        		}) ;; end _dataMineExtents
;;         (dataMineLib  _dataMineExtents)		Returns		true
;;
;; Notes: The start up script should contain code to this effect, or 
;;        the client should issue code like this at startup.
;;
;; Primary Repository:
;; Notes:    The data mine Primary repository is distributed across multiple
;;           extents, and houses the primary tables in the data mine. The
;;           primary table repository is a simple associative memory containing
;;           only the primary schema and all table record blocks. All table
;;           records are accumulated in blocks and are stored, in serial rotation,
;;           in each of the primary extents (i.e. block 1 is assigned to extent 1
;;           block 2 is assigned to extent 2 using modular arithmetic). The schema
;;           and table blocks are given text keys as follows:
;;
;;                "_schema"	                The Primary Schema child repository.
;;                "_volumeLabel{extentIndex}"	The volume label for each primary extent.
;;                "tablename_{Positive Number}"	The table blocks are assigned sequential numbers.
;;                "tablename_{Negative Number}"	All temporary table blocks are assigned negative numbers.
;;
;;           The Primary Schema is housed in a child repository to
;;           allow faster schema access, and to provide a single locking
;;           point for the entire data mine. The schema is a simple 
;;           associative memory with data mine "object names" as its keys.
;;           Each data mine "object name" is associated with its 
;;           corresponding schema record. The schema record layouts are as follows:
;;
;;           table or view: bckBlockRoot        The retrieval key (root name only) for all backup blocks.
;;                          bckSchema           The table's backup schema (#void if no backup).
;;                          blockCount	        The table's current block count.
;;                          blockSize 	        The number of records per table block.
;;                          ckpBlockRoot        The retrieval key (root name only) for all checkpoint blocks.
;;                          ckpSchema           The table's directory of check point schema (empty if no check point).
;;                          colCount	        The number of columns in the table.
;;                          colVector	        The table's vector of column names.
;;                          myObjectName        The table's name (identical to its data mine object name).
;;                          myObjectType        The object's type (table or view).
;;                          myTableName         The name of the table for this view.
;;                          myTableDate         The last update date of the table for this view.
;;                          newBlockRoot        The retrieval key (root name only) for all new blocks.
;;                          recordCount	        The table's current total record count.
;;                          recordStructure	The table's record structure (an attributed vector).
;;                          rowOrderSW	        The view is sorted by row order (views only).
;;                          tmpBlockRoot        The retrieval key (root name only) for all temporary block.
;;                          validFields   	The table's valid fields and types (necessary for query building).
;;                          viewFilter   	The view's initial filter (necessary for view resynching).
;;
;; Depends:  browseLib
;;           rulesLib
;;           securityLambda
;; Args:     none
;; Return:   true
;; *******************************************************************
  pvars:(autoNameSerial          ;; Unique modular automatic name serial number.
         blackboardExtCount      ;; Number of blackboard repository extents.
         blackboardRep           ;; The Vector of blackboard repository extents.
         blackboardSchema        ;; Blackboard repository extents schema (child repository).
         blockSizeFactor         ;; Factor used in computing block size.
         currentTable            ;; Holds the current primary table cursor.
         dmExt                   ;; The Structure of data mine extent path and file names.
         emptyCursor             ;; Holds the Dictionary of empty primary table update cursors.
         myParent                ;; Holds the parent Lambda object reference.
         primaryExtCount         ;; Number of primary repository extents.
         primaryRep              ;; The Vector of primary repository extents.
         primarySchema           ;; Primary repository extents schema (child repository).
         usingExtCount           ;; Number of repository extents (current area).
         usingRep                ;; The Vector of repository extents (current area).
         usingSchema             ;; Repository extents schema (child repository) (current area).
         updateCursor            ;; Holds the Dictionary of active primary table update cursors.
         ;; public methods
         addColumns              ;; Adds the specified columns to the specified table.
         addMinerChild           ;;
         addOffLimits            ;;
         addProjectChild         ;;
         addTableChild           ;; 
         backupTable             ;; Create a backup copy of the specified data mine table.
         checkin                 ;; Stores and compiles an Lambda source into the data mine.
         checkinNotes            ;;
         checkout                ;; Returns the Lambda source stored for a given Lambda.
         checkoutNotes           ;; Returns the Lambda notes stored for a given Lambda.
         checkpointTable         ;; Create a check point copy of the specified data mine table.
         clearAutoFilters        ;; Clears the un-named filter Lambdas in the specified miner.
         clearFilters            ;; Clears all filter Lambdas in the specified miner.
         clearScores             ;; Clears the scores in a specified miner.
         compileFilter           ;; Compiles the source string and returns a filter Lambda.
         compileGoal             ;; Compiles the source string and returns a goal Lambda.
         createTable             ;; Creates an entry for the specified primary table.
         createView              ;;
         dmMemoryCursor          ;; Cursor manager for in memory relational tables 
         dmTableCursor           ;; Cursor manager for disk resident relational tables 
         dropColumns             ;; Deletes the specified columns from the specified table in 
                                 ;; the data mine.
         dropObject              ;; Deletes the specified object from the data mine.
         dropMinerChild          ;; Deletes the specified object from a specified miner.
         dropOffLimits           ;;
         dropProjectChild        ;; Deletes the specified object from a specified project.
         dropTableChild          ;; Deletes the specified object from a primary table
         exportTab               ;; Exports the specified ASCII tab delimited file from 
                                 ;; the table in the data mine.
         getColumnCount          ;; Returns the number of columns in the specified table.
         getColumnNames          ;; Returns a structure of column names from a specified table.
         getColumnTypes          ;; Returns a structure of column names and types from a table.
         getDelimitedCells       ;; Returns a tabbed string of the table containing the values 
                                 ;; in the specified block of cells. The column names will be  
                                 ;; the first row in the ASCII tab delimited string.
         getDelimitedColumns     ;; Returns a tab-delimited string of column names from a specified table.
         getDelimitedObjects     ;; Returns a tab-delimited string of objects in the data mine.
         getDelimitedScores      ;; Returns the tab delimited scores in the specified miner.
         getMinerChildTab        ;; 
         getMinerGoal            ;; Returns the structure of a goal in a miner..
         getObjectNames          ;; Returns the names of objects in the data mine.
         getProjectChildTab      ;;
         getProjectMiner         ;;
         getScoredFilters        ;;
         getTablesTab            ;;
         getTableChildTab        ;;
         getTableTypes           ;; Returns a structure of table names and their table types.
         getTableViews           ;; Returns a structure of view names for a primary table.
         importTab               ;; Imports an ASCII tab-delimited file into a data mine table.
         mergeTables             ;; Merges two tables based upon a matching merge field.
         openProject             ;; Opens the specified project in the Data Mine
         openTable               ;;
         readRow                 ;; Reads the specified row from the table in the data mine.
         rename                  ;; Changes the name of the specified object in the data mine.
         restoreTable            ;; Restore the backup copy of the specified data mine table.
         rollbackTable           ;; Restore the check point copy of the specified data mine table.
         rowCount                ;; Returns the number of rows in the specified table. 
         runLambda                ;; Runs the named Lambda for a specified table in the data mine.
         scoreFilter             ;; Scores one or all filter Lambdas in a specified miner.
         setColumnTypes          ;; Assigns column types to a table in the data mine.          
         setFactoryLambda         ;; Turns on/off a factory Lambda in a specified miner.
         setFitnessGoal          ;; Sets the fitness goal in the specified miner.
         sort                    ;; Sorts a table in the data mine by the specified order.
         sortToView              ;; Sorts the specified table in the data mine to a view.
         startMining             ;; Resumes the backtesting process for the specified miner.
         statistics              ;; Returns the tab delimited statistics for the specified table and fields.
         truncate                ;; Truncates a set of rows from the specified table in the data mine.
                                 ;; Optionally leaves the table unchanged and places the logical results of the
                                 ;; truncation in a new view table, which may be system- or user-named.
         updateBegin             ;; Begins a transaction with the specified table in the data mine.
         updateEnd               ;; Terminates a transaction with the specified table in the data mine.
         updateRead              ;; Reads the specified row from the table in the data mine.
         updateWrite             ;; Writes the specified row to the table in the data mine.
         validateACL             ;;
         writeRow                ;; Writes the specified row to the table in the data mine (sans transaction).
         ;; private methods
         __clearDataMine         ;; Initializes the data mine multiple repository extents.
         __getFilterSource       ;; Return the ACL source for the named filter up Lambda.
         __getFixupSource        ;; Return the ACL source for the named fix up Lambda.
         __getGoalSource         ;; Return the ACL source for the named goal Lambda.
         __newAutoName           ;; Returns a new data mine automatic name (guaranteed to be unique).
         __updateBegin           ;; Begins a transaction with the specified table in the data mine (does not return an error).
         __usingForCreate        ;; Sets whether new objects are created in the primary or blackboard areas.
         )   ;;end of persistant variables
;; If no user name, generate a new name automatically.
  vars:(indexOf cursorName cursor)
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistent storage of the original Lambda.
    (if (<> addColumns #void) (goto Continue:))
    ;; Initialize the inline child Lambdas.
    ;; Returns a new data mine automatic name (guaranteed to be unique).
    (defun __newAutoName() 
       (setq autoNameSerial (modi (add1 autoNameSerial) 100000)) 
       (symbol (append "_Auto__" autoNameSerial "_" (now)))) ;; end __newAutoName
    ;; Return the ACL source for the named filter Lambda.
    (defun __getFilterSource(name) (error "unknownLambda"))
    ;; Return the ACL source for the named fix up Lambda.
    (defun __getFixupSource(name) (error "unknownLambda"))
    ;; Return the ACL source for the named goal Lambda.
    (defun __getGoalSource(name) (error "unknownLambda"))
    ;; summary:  Begins a transaction with the specified table in the data mine (does not return an error).
    (defun __updateBegin(tableName) (onError (lambda(s) #void)) (updateBegin tableName))
    ;; Sets whether new objects are created in the primary or blackboard areas.
    (defun __usingForCreate(location) 
       (if (= location primary:)
           (begin
              (setq usingExtCount primaryExtCount)
              (setq usingExt primaryExt)
              (setq usingRep primaryRep)
              (setq usingSchema primarySchema)
              )
           (begin
              (setq usingExtCount blackboardExtCount)
              (setq usingExt blackboardExt)
              (setq usingRep blackboardRep)
              (setq usingSchema blackboardSchema)
           )) ;; end if
       true) ;; end __usingForCreate
    ;; Create a backup copy of the specified data mine table.
    (defun backupTable(tableName)
       vars:(cursor)
       (setq cursor (updateBegin tableName))
       (cursor.backup)
       (updateEnd cursor)
       true) ;; end of backupTable
    ;; Creates a check point copy of the specified data mine table.
    (defun checkpointTable(tableName key)
       vars:(cursor)
       (setq cursor (updateBegin tableName))
       (cursor.checkPoint key)
       (updateEnd cursor)
       true) ;; end of checkpointTable
    (defun clearAutoFilters(minerName)
       ;; *******************************************************************
       ;; summary:  Clears the un-named filter Lambdas in the specified miner.
       ;;           
       ;;
       ;; Args:     minerName      name of the miner to clear auto filters in.
       ;;           
       ;;
       ;; Return:   True
       ;;               
       ;; *******************************************************************

       ;;
       ;;
       true) ;; end of clearAutoFilters
    (defun clearFilters(minerName)
       ;; *******************************************************************
       ;; summary:  Clears all filter Lambdas in the specified miner.
       ;;           
       ;;
       ;; Args:     minerName      name of the miner to clear all filters in.
       ;;           
       ;;
       ;; Return:   True
       ;;               
       ;; *******************************************************************

       ;;
       ;;
       true) ;; end of clearFilters
    (defun clearScores(minerName)
       ;; *******************************************************************
       ;; summary:  Clears the scores in a specified miner.
       ;;           
       ;;
       ;; Args:     minerName      name of the miner to clear all scores in.
       ;;           
       ;;
       ;; Return:   True
       ;;               
       ;; *******************************************************************

       ;;
       ;;
       true) ;; end of clearScores
    (defun compileFilter(tableName source)
       ;; *******************************************************************
       ;; summary:  Compiles the source string and returns a filter Lambda.
       ;;           All filter Lambdas are bound to the table they are to filter
       ;;           
       ;;
       ;; Args:     tableName      Name of the table to bind the filter Lambda to.
       ;;           source         Filter ACL source string of the filter Lambda.
       ;;           
       ;;
       ;; Return:   Filter Lambda reference
       ;;               
       ;; *******************************************************************

       ;;
       ;;
       #void) ;; end of compileFilter
    (defun compileGoal(tableName source)
       ;; *******************************************************************
       ;; summary:  Compiles the source string and returns a goal Lambda.
       ;;           All goal Lambdas are bound to the table they reference
       ;;           
       ;;
       ;; Args:     tableName      Name of the table to bind the goal Lambda to.
       ;;           source         Goal ACL source string of the goal Lambda.
       ;;           
       ;;
       ;; Return:   Goal Lambda reference
       ;;               
       ;; *******************************************************************

       ;;
       ;;
       #void) ;; end of compileGoal
    (defun createView(tableName ...)
       ;; *******************************************************************
       ;; summary:  Create the specified table in the datamine.
       ;;           
       ;;           
       ;;
       ;; Args:     tableName      Name of the table to create
       ;;           ...            zero or more column specs
       ;;           
       ;;
       ;; Return:   true
       ;;               
       ;; *******************************************************************

       ;;
       ;;
       true) ;; end of createTable
    (defun dropColumns(tableName ...)
       ;; *******************************************************************
       ;; summary:  Deletes the specified columns from the specified table in the data mine.
       ;;           
       ;;           
       ;;
       ;; Args:     tableName      Name of the table from which to delete columns
       ;;           ...            zero or more column names to delete
       ;;           
       ;;
       ;; Return:   true
       ;;               
       ;; *******************************************************************

       ;;
       ;;
       true) ;; end of dropColumns
    (defun exportTab(tableName asciiFile ...)
       ;; *******************************************************************
       ;; summary:  Exports the specified ASCII tab delimited file from the 
       ;;           table in the data mine.
       ;; Args:     tableName      Name of the table from which to export data
       ;;           asciiFile      Fully-qualified pathname of the file to export
       ;;           arg3           Structure containing names of columns to export
       ;;           arg4           1-based row number of first row to export
       ;;           arg5           1-based row number of last row to export
       ;; Return:   true
       ;; *******************************************************************
       vars:(cursor)
       (setq cursor (updateBegin tableName))
       (cursor.exportTab asciiFile)
       (updateEnd cursor)
       true) ;; end of exportTab
    (defun importTab(tableName option asciiFile ...)
       ;; *******************************************************************
       ;; summary:  Imports the specified ASCII tab delimited file into the 
       ;;           table in the data mine. 
       ;; Args:     tableName      Name of the table from which to export data
       ;;           option         If overwrite: the table is overwritten, 
       ;;                          if append: the rows are added to the end of the table
       ;;           asciiFile      Fully-qualified pathname of the file to export
       ;;           arg4           If IsString, path to file containing names of columns
       ;;                          If IsStructure, the structure contains the column names
       ;;                          If #void, the 1st row of the import file contains the column names
       ;;           arg5           1-based row number of first row to import
       ;;           arg6           1-based row number of last row to import
       ;; Return:   true
       ;; *******************************************************************
       vars:(cursor)
       (setq cursor (updateBegin tableName))
       (if (= option overwrite:) (cursor.updateDrop))
       (cursor.importTab asciiFile)
       (updateEnd cursor)
       true) ;; end of importTab
    (defun readRow(tableName rowNumber ...)
       ;; *******************************************************************
       ;; summary:  Reads the specified row from the table in the data mine. 
       ;;
       ;;
       ;;
       ;; Args:     tableName       Table from which the row is to be read
       ;;           rowNumber       1-based number of row to be read
       ;;           arg3            If missing, return a dictionary for a sparse table,
       ;;                           or a structure for a non-sparse table
       ;;                           If structure:, return a record structure,
       ;;                           even in table's sparse option is true
       ;;                           If aStructure, fill the matching attributes with
       ;;                           their respective field values
       ;;
       ;;
       ;; Return:   structure or dictionary of row values
       ;;
       ;; *******************************************************************

       ;;
       ;;
       #void) ;; end of readRow
    (defun rename(oldName newName)
       ;; *******************************************************************
       ;; summary:  Changes the name of the specified object in the data mine. 
       ;;           
       ;;           
       ;;
       ;; Args:     oldName       Object to rename
       ;;           newName       New name for object
       ;;                           
       ;;           
       ;;
       ;; Return:   true
       ;;
       ;; *******************************************************************

       ;;
       ;;
       true) ;; end of rename
    ;; Restores the backup copy of the specified data mine table.
    (defun restoreTable(tableName)
       vars:(cursor)
       (setq cursor (updateBegin tableName))
       (cursor.restore)
       (updateEnd cursor)
       true) ;; end of restoreTable
    ;; Restores the check point copy of the specified data mine table.
    (defun rollbackTable(tableName key)
       vars:(cursor)
       (setq cursor (updateBegin tableName))
       (cursor.rollBack key)
       (updateEnd cursor)
       true) ;; end of rollbackTable
    (defun rowCount(tableName)
       ;; *******************************************************************
       ;; summary:  Returns the number of rows in the specified table.
       ;;           
       ;;           
       ;;
       ;; Args:     tableName     Table from with to return the number of rows.
       ;;                           
       ;;           
       ;;
       ;; Return:   Number of rows in the specified table (1-based)
       ;;
       ;; *******************************************************************

       ;;
       ;;
       500) ;; end of rowCount
    (defun runLambda(LambdaName tableName)
       ;; *******************************************************************
       ;; summary:  Runs the named Lambda for the specified table in the data mine.
       ;;           
       ;;           
       ;;
       ;; Args:     LambdaName     Name of the Lambda (within the data mine) to execute
       ;;           tableName     Table argument to be passed o the Lambda
       ;;                           
       ;;           
       ;;
       ;; Return:   The value returned by the named Lambda
       ;;
       ;; *******************************************************************

       ;;
       ;;
       #void) ;; end of runLambda
    (defun statistics(arg1 arg2 ...)
       ;; *******************************************************************
       ;; summary:  Returns the tab delimited statistics for the specified 
       ;;           table and fields.
       ;;           
       ;;
       ;; Args:     Too complicated to do right now
       ;;           
       ;;
       ;; Return:   TDS of statistics on the specified args
       ;;
       ;; *******************************************************************

       ;;
       ;;
       (append "stats:" #\tab "100" #\tab "200")) ;; end of statistics
    (defun updateBegin(tableName ...)
       ;; *******************************************************************
       ;; summary:  Begins a transaction with the specified table in the data mine.
       ;; Args:     tableName    The table on which to operate
       ;;           memory:      A memory cursor is to be opened.
       ;; Return:   cursor       The transaction cursor
       ;; *******************************************************************
       vars:(cursor cursorName colnames)
       ;; Open a memory based cursor?
       (if (and (>= (argCount) 2) (= (argFetch 1) memory:))
           (begin
              (setq cursorName (__newAutoName))
              (setq cursor (new dmMemoryCursor))
              (setq cursor.Pv.myCursorName cursorName)
              (cursor myParent)
              ;; Has the caller specified a reduced set of columns?
              (if (= (argCount) 3)
                  ;; Open memory cursor with a reduced set of columns.
                  (cursor.updateBegin tableName (argFetch 2))
                  ;; Open memory cursor with the table's set of columns.
                  (cursor.updateBegin tableName)
                  ) ;; end if
              (setq updateCursor[cursorName] cursor)
              (return cursor))) ;; end memory cursor if
       ;; Open a disk based cursor.
       (if (= (length emptyCursor) 0)
           (begin
              (setq cursorName (__newAutoName))
              (setq cursor (new dmTableCursor))
              (setq cursor.Pv.myCursorName cursorName)
              (setq emptyCursor[cursorName] cursor)
           )) ;; end if
       (setq cursor emptyCursor[0 1])
       (setq cursorName cursor.myCursorName)
       (setq emptyCursor[cursorName] #void)
       (setq updateCursor[cursorName] cursor)
       (cursor myParent)
       (cursor.updateBegin tableName)
       cursor) ;; end of updateBegin
    (defun updateEnd(cursor)
       ;; *******************************************************************
       ;; summary:  Ends the current transaction
       ;; Args:     cursor   The transaction cursor
       ;; Return:   true
       ;; *******************************************************************
       (cursor.__closeTableCursor)
       (cursor.updateEnd)
       #void) ;; end of updateEnd
    (defun updateRead(cursor rowNumber ...)
       ;; *******************************************************************
       ;; summary:  Reads the specified row from the table in the data mine.
       ;; Args:     cursor       The transaction cursor
       ;;           rowNumber    1-based number of the row to read
       ;;           arg2         If structure:, return a structure containing
       ;;                        the row
       ;;                        If missing, return a dictionary for sparse
       ;;                        tables, return a structure for non-sparse
       ;;                        If a structure object, fill in the matching
       ;;                        attributes with field values.
       ;; Return:   record read from table
       ;; *******************************************************************
       (cursor.updateRead rowNumber)) ;; end of updateRead
    (defun updateWrite(cursor rowNumber rowData)
       ;; *******************************************************************
       ;; summary:  Writes the specified row to the table in the data mine.
       ;; Args:     cursor       The transaction cursor name
       ;;           rowNumber    1-based number of the row to write
       ;;           rowData      Data to write, may be a dictionary, structure
       ;;                        or vector, in which case columns are written in sequence
       ;; Return:   record written to table
       ;; *******************************************************************
       (cursor.updateWrite rowNumber rowData)) ;; end of updateWrite
    (defun writeRow(tableName rowNumber rowData)
       ;; *******************************************************************
       ;; summary:  Writes the specified row to the table in the data mine 
       ;;           (sans transaction).
       ;;
       ;; Args:     tableName    Table to be written to.
       ;;           rowNumber    1-based row number to write to.
       ;;           rowData      Data to write, either in structure, dictionary
       ;;                        or vector.
       ;;
       ;;
       ;; Return:   true
       ;;
       ;; *******************************************************************

       ;;
       ;;
       true) ;; end of writeRow
    ;; Initialize the dataMineLib
    Continue::
    ;; Establish the multiple extent repositories.
    (setq blockSizeFactor 90000)
    (setq dmExt dataMineExtents)
    (if (<> (type dmExt) Structure:) (error "dataMineLib" "dataMineExtents not installed properly"))
    (setq currentTable #void)
    (setq updateCursor (new Dictionary:))
    (setq emptyCursor (new Dictionary:))
    (setq cursorName (__newAutoName))
    (setq cursor (new dmTableCursor))
    (setq cursor.Pv.myCursorName cursorName)
    (setq emptyCursor[cursorName] cursor)
    (setq primaryRep (new Vector: 0))
    (setq blackboardRep (new Vector: 0))
    (setq primaryExtCount (length dmExt.primary))
    (setq blackboardExtCount (length dmExt.blackboard))
    (setq myParent (myself))
    (loop for indexOf from 0 until primaryExtCount do
        (setq primaryRep[indexOf] (new ObjectRepository: dmExt.primary[indexOf]))
        ) ;; end primary repository loop
    (loop for indexOf from 0 until blackboardExtCount do
        (setq blackboardRep[indexOf] (new ObjectRepository: dmExt.blackboard[indexOf]))
        ) ;; end blackboard repository loop
    ;; Check for empty multiple repositories and initialize (if necessary).
    (if (= (length primaryRep[0]) 0)
        (begin
           (loop for indexOf from 0 until primaryExtCount do
               (if (<> (length primaryRep[indexOf]) 0) 
                   (error "dataMineLib" "Primary extent [" indexOf "] invalid."))
               ) ;; end primary repository loop
           (loop for indexOf from 0 until blackboardExtCount do
               (if (<> (length blackboardRep[indexOf]) 0) 
                   (error "dataMineLib" "Blackboard extent [" indexOf "] invalid."))
               ) ;; end blackboard repository loop
           (__clearDataMine)
           )) ;; end if
    ;; Check the multiple repositories for validity.
    (loop for indexOf from 0 until primaryExtCount do
        (if (<> primaryRep[indexOf]._volumeLabel (append "PrimaryVolume" indexOf)) 
            (error "dataMineLib" "Primary extent [" indexOf "] invalid."))
        ) ;; end primary repository loop
    (loop for indexOf from 0 until blackboardExtCount do
        (if (<> blackboardRep[indexOf]._volumeLabel (append "BlackboardVolume" indexOf)) 
            (error "dataMineLib" "Blackboard extent [" indexOf "] invalid."))
        ) ;; end blackboard repository loop
    (if (<> (type primaryRep[0]._schema) ObjectRepository:) 
        (error "dataMineLib" "Primary extent [" 0 "] invalid."))
    (if (<> (type blackboardRep[0]._schema) ObjectRepository:) 
        (error "dataMineLib" "Blackboard extent [" 0 "] invalid."))
    ;; Establish direct links to the schema child repositories.
    (setq primarySchema primaryRep[0]._schema)
    (setq blackboardSchema blackboardRep[0]._schema)
    ;; Set the current area to the primary area.
    (__usingForCreate primary:)
    true) ;; end of dataMineLib













;;**EXPORTKEY**:dataMineLib:__clearDataMine
(defChildLambda dataMineLib:__clearDataMine()
    ;; *******************************************************************
    ;; summary:  Clear the data mine.
    ;;           
    ;; Args:     none
    ;;
    ;; Return:   true
    ;; *******************************************************************
    vars:(indexOf (schemaSize 1000000))
    ;; Clear the multiple repositories and initialize.
    (setq currentTable #void)
    (loop for indexOf from 0 until primaryExtCount do
        (clear primaryRep[indexOf])
        (setq primaryRep[indexOf]._volumeLabel (append "PrimaryVolume" indexOf))
        ) ;; end primary repository loop
    (loop for indexOf from 0 until blackboardExtCount do
        (clear blackboardRep[indexOf])
        (setq blackboardRep[indexOf]._volumeLabel (append "BlackboardVolume" indexOf))
        ) ;; end blackboard repository loop
    ;; Clear the repositories schema child repositories.
    (saveRepository primaryRep[0] _schema: schemaSize)
    (saveRepository blackboardRep[0] _schema: schemaSize)
    (setq primarySchema primaryRep[0]._schema)
    (setq blackboardSchema blackboardRep[0]._schema)
    (__usingForCreate primary:)
    true) ;; end of __clearDataMine













;;**EXPORTKEY**:dataMineLib:addColumns
(defChildLambda dataMineLib:addColumns(tableName ...)
   ;; *******************************************************************
   ;; summary:  Add columns to the specified table. An indefinite list of
   ;;           columns may be passed.
   ;;
   ;; Style 1 Args:     tableName      name of table
   ;;                   col1...colN    column names 1 thru N
   ;;
   ;; Style 2 Args:     tableName                            name of table
   ;;                   '(col1 . type1)...'(colN . typeN)    name.type pairs 1 thru N
   ;;
   ;; Return:   True
   ;; 
   ;; *******************************************************************
   ;;
   ;;
   true) ;; end of addColumns













;;**EXPORTKEY**:dataMineLib:addMinerChild
(defChildLambda dataMineLib:addMinerChild(minerName objType objName ...)
   ;; *******************************************************************
   ;; summary:  Adds a the specified filter Lambda to the list filters
   ;;           for the specified miner.
   ;;
   ;; Args:     minerName      name of the miner to receive the testing table
   ;;           objType        type of the child object to be added
   ;;           objName        name of the child object to be added
   ;;           arg3           optional project name
   ;;
   ;; Return:   True
   ;;
   ;; *******************************************************************
   ;;
   ;; Test to see if project is provided
   (if (= (argCount) 3) 
      ;; Look for open project
      (if (= projectLambda.Pv.currentProject[name:] #void) (error {projectRequired}))
   else
      ;; Open project name provided
      (if (<> (openProject (argFetch 3)) true) (return #void))
   )
   ;;
   ;; Evaluate type and add to miner
   (projectLambda.addMinerObject minerName objType objName))
   ;;
   ;; end of addMinerChild












;;**EXPORTKEY**:dataMineLib:addOffLimits
(defChildLambda dataMineLib:addOffLimits(goalName fieldName ...)
    ;; *******************************************************************
    ;; summary:  Adds a the specified field to the specified goal Lambda's
    ;;           offlimits field list.
    ;;
    ;; Args:     goalName      name of the receiver object. 
    ;;           fieldName     name of the field to add to the offlimits list
    ;;           projectName   name of project containing goal Lambda
    ;;           
    ;;
    ;; Return:   True
    ;;
    ;; *******************************************************************
    ;;
    ;;
    (if (= (argCount) 3)
       ;; Project supplied
       (if (<> (openProject projName) true) (return #void))
    )
    (measureCriteriaLambda.addNewExcludeName goalName fieldName)
    true) ;; end of addOffLimits













;;**EXPORTKEY**:dataMineLib:addProjectChild
(defChildLambda dataMineLib:addProjectChild(objType objName ...)
    ;; *******************************************************************
    ;; summary:  Adds a the specified object to the specified project.
    ;;
    ;; Args:     objType        type of the object 
    ;;           objName        name of object to be added
    ;;           arg2           optional project name
    ;;
    ;; Return:   True
    ;;
    ;; *******************************************************************
    ;;
    ;; Test for project
    (if (= (argCount) 2)
       (if (= projectLambda.Pv.currentProject[name:] #void) (error {projNotOpen}))
    else
       (if (<> (openProject (argFetch 2)) true) (return #void))
    )
    ;;
    ;; Add to project
    (projectLambda.addProjectObject projectLambda.Pv.currentProject[name:] objType objName)) ;; end of addProjectChild













;;**EXPORTKEY**:dataMineLib:addTableChild
(defChildLambda dataMineLib:addTableChild(objType objName ...)
    ;; *******************************************************************
    ;; summary:  Adds a the specified object to the specified table.
    ;;
    ;; Args:     objType        type of the object 
    ;;           objName        name of object to be added
    ;;           arg2           optional table name
    ;;
    ;; Return:   True
    ;;
    ;; *******************************************************************
    ;;
    ;; Test for project
    (if (= (argCount) 2)
       (if (= currentTable #void) (error {tableNotOpen}))
    else
       (if (<> (openTable (argFetch 2)) true) (return #void))
    )
    ;;
    ;; Add to datamine
    (cond ((= objType project:)
             (projectLambda.addProject objName currentTable)
          )
          (else (error {invalidType}))
    ;;
    )) ;; end of addTableChild













;;**EXPORTKEY**:dataMineLib:checkin
(defChildLambda dataMineLib:checkin(LambdaType LambdaSource LambdaNotes LambdaName)
   ;; *******************************************************************
   ;; summary:  Stores and compiles an Lambda source into the data mine.
   ;;           
   ;;
   ;; Args:     LambdaType      Valid types are goal and filter
   ;;           LambdaSource    The source string for the Lambda.
   ;;           LambdaNotes     The notes string for the Lambda
   ;;           LambdaName      The name of the Lambda to be checked in.
   ;;                          If the name is #void, then an automatic
   ;;                          name shall be generated.
   ;;
   ;; Return:   The name of the Lambda after checkin
   ;;
   ;; *******************************************************************
   vars:(result)
   ;;
   (if (= LambdaName #void) (setq result (setq LambdaName (append "zz_" (string (now))))))
   ;;
   (if (= LambdaType goal:)
      (begin
         (setq result (measureCriteriaLambda.addNewCriteria LambdaSource LambdaName LambdaNotes))
      )
   else
      (begin
         (setq result (selectCriteriaLambda.addNewCriteria LambdaSource LambdaName LambdaNotes))
      )
   )
   ;;
   result) ;; end of checkin













;;**EXPORTKEY**:dataMineLib:checkinNotes
(defChildLambda dataMineLib:checkinNotes(LambdaType LambdaName LambdaNotes)
   ;; *******************************************************************
   ;; summary:  Stores and compiles an Lambda source into the data mine.
   ;;           
   ;;
   ;; Args:     LambdaType      Valid types are goal and filter
   ;;           LambdaName      The name of the Lambda to be checked in.
   ;;           LambdaNotes     The notes string for the Lambda
   ;;
   ;; Return:   The name of the Lambda after checkin
   ;;
   ;; *******************************************************************
   vars:(result)
   ;;
   (if (or (= LambdaType goal:) (= LambdaType goalLambda:))
      (begin
         (setq result (measureCriteriaLambda.saveCriteriaNotes LambdaName LambdaNotes))
      )
   else
      (begin
         (setq result (selectCriteriaLambda.saveCriteriaNotes LambdaName LambdaNotes))
      )
   )
   ;;
   result) ;; end of checkinNotes













;;**EXPORTKEY**:dataMineLib:checkout
(defChildLambda dataMineLib:checkout(LambdaName ...)
   ;; *******************************************************************
   ;; summary:  Returns the Lambda source specified from the data mine.
   ;;           
   ;;
   ;; Args:     LambdaName      The name of the Lambda to be checked out.
   ;;           arg1           Optional flag to indicate type of Lambda.
   ;;
   ;;
   ;; Return:   The source string of the Lambda.
   ;;               
   ;; *******************************************************************
   vars:(
          srcString            ;; source string of Lambda
        )
   ;;
   ;;
   (setq srcString (if (or (= (argCount) 1) (= (argFetch 1) goal:))
                      (measureCriteriaLambda.getCriteria LambdaName)
                   else
                      (selectCriteriaLambda.getCriteria LambdaName)
                   )
   )
   srcString) ;; end of checkout













;;**EXPORTKEY**:dataMineLib:checkoutNotes
(defChildLambda dataMineLib:checkoutNotes(LambdaName ...)
   ;; *******************************************************************
   ;; summary:  Returns the Lambda source specified from the data mine.
   ;;           
   ;;
   ;; Args:     LambdaName      The name of the Lambda notes to be checked out.
   ;;
   ;;
   ;; Return:   The notes string of the Lambda.
   ;;               
   ;; *******************************************************************
   vars:(
          noteString            ;; notes string of Lambda
        )
   ;;
   ;;
   (setq noteString (if (or (= (argCount) 1) (= (argFetch 1) goal:))
                      (measureCriteriaLambda.loadCriteriaNotes LambdaName)
                   else
                      (selectCriteriaLambda.loadCriteriaNotes LambdaName)
                   )
   )
   noteString) ;; end of checkoutNotes













;;**EXPORTKEY**:dataMineLib:createTable
(defChildLambda dataMineLib:createTable(tableName ...)
    ;; *******************************************************************
    ;; summary:  Create the specified table in the datamine.
    ;; Args:     tableName      Name of the table to create
    ;;           ...            zero or more column specs
    ;; Return:   true
    ;; *******************************************************************
    vars:(indexOf schema colVector colCount recordStructure validFields)
    ;; Collect any column names.
    (setq colVector (new Vector: object: 0))
    (loop for indexOf from 1 until (argCount) do
        (setq colVector[(sub1 indexOf)] (symbol (argFetch indexOf)))
        ) ;; end of column name loop
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
    (if (<> primarySchema[(symbol tableName)] #void) (error "createTable" "The specified table already exists."))
    (if (<> blackboardSchema[(symbol tableName)] #void) (error "createTable" "The specified table already exists."))
    ;; Save the new table schema record.
    (setq schema (new Structure:
                         bckBlockRoot:    #void
                         bckSchema:       #void
                         blockCount:	  0
                         blockSize: 	  (divi blockSizeFactor (max colCount 1))
                         ckpBlockRoot:    #void
                         ckpSchema:       (new Directory:)
                         colCount:        colCount
                         colVector:       colVector
                         myObjectName:    (symbol tableName)
                         myObjectType:    table:
                         myTableName:     #void
                         myTableDate:     #void
                         newBlockRoot:    (__newAutoName)
                         recordCount:     0
                         recordStructure: recordStructure
                         rowOrderSW:      false
                         tmpBlockRoot:    #void
                         validFields:     validFields
                         viewFilter:      #void
                         )) ;; end table schema record
    (setq usingSchema[(symbol tableName)] schema)
    true) ;; end of createTable













;;**EXPORTKEY**:dataMineLib:createView
(defChildLambda dataMineLib:createView(viewName tableName ...)
    ;; *******************************************************************
    ;; summary:  Create the specified table view in the datamine.
    ;; Args:     viewName      Name of the view to create
    ;;           tableName     Name of the table for this view
    ;;           ...           Truncate Lambda for initialization of this view
    ;; Notes:    Table views always have two predefined fields:
    ;;            __row        The corresponding table row number.
    ;;            __value      The corresponding table row's value,
    ;; Return:   true
    ;; *******************************************************************
    vars:(indexOf schema recordCount colVector colCount 
          recordStructure validFields
          tableCursor viewCursor 
          truncateLambda
          ) ;; end of temporary variables
    ;; Make sure the table exists by opening a cursor on it.
    (setq tableCursor (updateBegin tableName))
    ;; Capture the view filter source (if any).
    (if (= (argCount) 3) (setq truncateLambda (argFetch 2)))
    ;; Declare predefined view column names.
    (setq colVector (new Vector: object: 2 __row:  __value:))
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
    ;; Make sure the specified view does not exist
    (if (<> primarySchema[(symbol viewName)] #void) (error "createView" "The specified view already exists."))
    (if (<> blackboardSchema[(symbol viewName)] #void) (error "createView" "The specified view already exists."))
    ;; Save the new table schema record.
    (setq schema (new Structure:
                         bckBlockRoot:    #void
                         bckSchema:       #void
                         blockCount:	  0
                         blockSize: 	  (divi blockSizeFactor (max colCount 1))
                         ckpBlockRoot:    #void
                         ckpSchema:       (new Directory:)
                         colCount:        colCount
                         colVector:       colVector
                         myObjectName:    (symbol viewName)
                         myObjectType:    view:
                         myTableName:     (symbol tableName)
                         myTableDate:     #void
                         newBlockRoot:    (__newAutoName)
                         recordCount:     0
                         recordStructure: recordStructure
                         rowOrderSW:      #void
                         tmpBlockRoot:    #void
                         validFields:     tableCursor.validFields
                         viewFilter:   	  truncateLambda
                         )) ;; end table schema record
    (setq usingSchema[(symbol viewName)] schema)
    (updateEnd tableCursor)
    ;; Resynchronize the view with table records. Opening a cursor
    ;; on the view will perform the resynchronize operation.
    (setq viewCursor (updateBegin viewName))
    (updateEnd viewCursor)
    true) ;; end of createView


















;;**EXPORTKEY**:dataMineLib:dmMemoryCursor
(defriend dataMineLib:dmMemoryCursor(dataMine)
;; *******************************************************************
;; Summary:  Data Mine table cursor which manages a table in
;;           the data mine schema. This table cursor is built 
;;           on principles similar to relational cursors, but
;;           loads a complete copy of the table in memory.
;; Notes:    Each element in the table is called a record.
;;           The index of a table record is called its row (rows begin with zero).
;; Depends:  dataMineLib
;; args:     dataMine	    The data mine Lambda owning this cursor.
;; Return:   self           This newly initialized table cursor.
;; *******************************************************************
    ;; Persistent Variables
    pvars:(fileID              ;; The file id used during import and export.
           myCursorName        ;; The unique name of this data mine table cursor.
           myCursorType        ;; The type of this data mine memory cursor (memory).
           myDataMine          ;; The data mine owner of this cursor.
           myExtCount          ;; Number of repository extents.
           myIndex             ;; The schema repository for data mine tables.
           myParent            ;; This parent cursor Lambda.
           myRepos             ;; This vector of data mine table repositories.
           myTableCursor       ;; The cursor for the table of this view (views only).
           myType              ;; The type of this data mine cursor.
           newBlockNUM         ;; The number of the new block.
           newBlockEXT         ;; The extent number for the new block.
           newBlockIDX         ;; The record index within the new block.
           newBlockKEY         ;; The retrieval key for the new block.
           showLimit           ;; The maximum number of records to display with the show function.
           ;; Schema globals (see dataMineLib table schema).
           ;; Note: We load the schema into our Pv to speed record processing.
           ;;       We name blocks by root name and block number. Block name key roots 
           ;;      (for current, temporary, backup, and checkpoint blocks) are stored here.
           mySchema            ;; The table object schema record (#void if cursor is inactive).
            bckBlockRoot       ;; The retrieval key (root name only) for all backup blocks.
            bckSchema          ;; The table's backup schema (#void if no backup).
            blockCount         ;; The table's current block count.
            blockSize          ;; The number of records per table block.
            ckpBlockRoot       ;; The retrieval key (root name only) for all checkpoint blocks.
            ckpSchema          ;; The table's directory of checkpoint schema (empty if no checkpoint).
            colCount           ;; The number of columns in the table.
            colVector          ;; The table's vector of column names.
            myObjectName       ;; The name of the data mine table or view object (#void if cursor is inactive).
            myObjectType       ;; The name of the data mine object type (#void if cursor is inactive).
            myTableName        ;; The name of the table for this view (#void if this is a table object).
            myTableDate        ;; The last update date of the table for this view (#void if this is a table object).
            newBlockRoot       ;; The retrieval key (root name only) for all new blocks.
            recordCount        ;; The table's current total record count.
            recordStructure    ;; The table's record structure.
            rowOrderSW         ;; The view is sorted by row order (views only).
            tmpBlockRoot       ;; The retrieval key (root name only) for all temporary block.
            validFields        ;; The table's valid fields and types (necessary for query compilation).
            viewFilter         ;; The view's initial filter (necessary for view resynching).
           ;; In memory record vectors and block lists.
           rowVector           ;; The current record vector.
           bckVector           ;; The backup record vector.
           ckpDirectory        ;; The directory of checkpoint record vectors.
           ;; Update transaction globals (see updateBegin and updateEnd).
           UPDcurrentBlock     ;; The current block of table records.
           UPDblockNUM         ;; The number of the current block.
           UPDblockEXT         ;; The extent number for the current block.
           UPDblockIDX         ;; The record index within the current block.
           UPDblockKEY         ;; The retrieval key for the current block.
           UPDIamDirty         ;; Update has occurred switch.
           UPDInProgress       ;; Update transaction in progress switch.
           ;; Public Child Lambdas
           backup              ;; Creates a backup copy of the data mine table.
           checkPoint          ;; Creates a check point copy of the data mine table.
           compileFilter       ;; Returns a compiled filter Lambda from the specified ACL source.
           delimitedCells      ;; Returns a block of cells as a tab delimited string.
           exportTab           ;; Exports ascii tab delimited records from the table.
           getColumnHeaders    ;; Returns the table headers as a tab delimited string.
           importTab           ;; Imports ascii tab delimited records into the table.
           refExport           ;; Returns a record, for export, to the exportTab function.
           refImport           ;; Returns an empty record to the importTab function.
           restore             ;; Restores the backup copy of the data mine table.
           rollBack            ;; Restores the check point copy of the data mine table.
           setImport           ;; Receives a single record from the importTab function.
           show                ;; Shows a group of records starting from the specified row.
           sort                ;; Sort the table rows using a lambda sort function.
           truncate            ;; Truncates a table to those records for which a lambda predicate is true.
           updateBegin         ;; Begin an update transaction on a table object.
           updateDrop          ;; Drop all records from a table object.
           updateEnd           ;; Terminate an update transaction on the table object.
           updateRead          ;; Read a table row for later update.
           updateSave          ;; Save the contents of the table cursor back to disk.
           updateWrite         ;; Write a table row, perhaps previously read, for update.
           ;; Private Child Lambdas
           __clear             ;; Clears the memory cursor.
           __closeTableCursor  ;; Closes the table cursor.
           __errorStop         ;; Handles error conditions during sensitive operations.
           __truncateEnds      ;; Truncate records from both ends of a table.
           __truncateMid       ;; Truncate records from the center of a table.
          ) ;; end of persistent variables
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> __clear #void) (goto Continue:))
    ;; Initialize the inline child Lambdas.
    (defun backup()
    ;; *******************************************************************
    ;; Summary: Creates a backup copy of the data mine table.
    ;; *******************************************************************
       (setq bckVector (copy rowVector))
       true) ;; end of backup
    (defun checkPoint(key)
    ;; *******************************************************************
    ;; Summary: Creates a check point copy of the data mine table.
    ;; *******************************************************************
       (setq ckpDirectory[key] (copy rowVector))
       true) ;; end of checkPoint
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
       (if (<= recordCount startRow) (return dls))
       (loop for rowIndex from startRow to endRow do
          (setq dls (append dls rowIndex (char 09)))
          (setq record (updateRead rowIndex))
          (loop for colIndex from startCol until endCol do
              (setq cellValue record[colIndex])
              (if (isString cellValue) (setq cellValue (substitute cellValue tab " ")))
              (if (= cellValue #void) (setq cellValue ""))
              (if (isVector cellValue) (setq cellValue (string cellValue true)))
              (setq cellValue (append "" cellValue)) 
              (setq cellValue (substitute cellValue (char 09)  " "))
              (setq cellValue (substitute cellValue (char 13)  " "))
              (setq dls (append dls cellValue (char 09)))
              ) ;;end column loop
          (setq dls (append dls (char 10) (char 13)))
          ) ;;end row loop
       (append dls "")) ;; end of delimitedCells
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
       (loop for colIndex from startCol until endCol do
           (setq colName (string colVector[colIndex]))
           (if (= (left colName 1) "|") (setq colName (mid colName 1 (subi (length colName) 2))))
           (setq dls (append dls colName (char 09)))
           ) ;;end column loop
       (append dls "")) ;; end getColumnHeaders
    (defun importTab(fileName)
    ;; *******************************************************************
    ;; Summary:  Performs an import tab transaction on the table cursor.
    ;; *******************************************************************
       ;; We do not allow import into views.
       (if (= myObjectType view:) (error (append "importTab: An attempt was made to import into a view: " myObjectName)))
       ;; Make sure we catch all errors which might occur.
       (onError __errorStop)
       ;; Open the specified .tab file and import.
       (setq fileID (fileOpen fileName 0 0))
       (^importTab fileID myParent recordVectors:)
       (fileClose fileID 1)
       (setq fileID #void)
       recordCount) ;; end importTab
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
       (updateRead row)) ;; end of refExport 
    (defun refImport(row)
    ;; *******************************************************************
    ;; Summary:  Returns an empty record to the importTab function.
    ;; *******************************************************************
       ;; If the row is zero, return an empty vector to hold the column names,
       ;; Otherwise, return an empty record (use the recordStructure as a template).
       (if (= row 0)
           (return (^new Vector: 0))
           (return (copy recordStructure)))) ;; end of refImport
    (defun restore()
    ;; *******************************************************************
    ;; Summary: Restores the backup copy of the data mine table.
    ;; *******************************************************************
       (if (= bckVector #void) (error (append "restore: There is no backup copy to restore.")))
       (setq rowVector (copy bckVector))
       (setq recordCount (length rowVector))
       true) ;; end of restore
    (defun rollBack(key)
    ;; *******************************************************************
    ;; Summary: Restores the check point copy of the data mine table.
    ;; *******************************************************************
       (if (= ckpDirectory[key] #void) (error (append "rollBack: There is no check point copy to roll back.")))
       (setq rowVector (copy ckpDirectory[key]))
       (setq recordCount (length rowVector))
       true) ;; end of rollBack
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
                  (setq blockSize (divi myDataMine.blockSizeFactor (max colCount 1)))
                  (return true)) ;; end setting new column names.
               (begin 
                 (setq recordVector (objectToStructure recordVector #(#void)))
                 (setq recordVector (refAttributes recordVector))
                 (if (<> colVector recordVector)
                      (error (append "importTab: An attempt was made to import a file with mismatched column names: " myObjectName)))
                  (return true)) ;; end checking new column names against old column names.
           )) ;; end if
       ;; Import all other .tab records at the end of the table.
       (updateWrite recordCount recordVector)
       true) ;; end of setImport
    (defun show(startIndex) 
    ;; *******************************************************************
    ;; Summary:  Shows a group of records starting from the specified row.
    ;; *******************************************************************
       vars:(i n) 
       (setq n (min recordCount (addi startIndex showLimit)))
       (loop for i from startIndex until n do
           (writeln "[" i "] " (updateRead i))
           ) ;; end loop
       true) ;; end show
    (defun sort(sortLambda valueLambda sortOp)
    ;; *******************************************************************
    ;; Summary:  Sorts each record in the table cursor.
    ;; *******************************************************************
       (if (<= recordCount 0) (return recordCount))
       (^sort rowVector sortLambda)
       recordCount) ;; end sort
    (defun truncate(findLambda)
    ;; *******************************************************************
    ;; Summary:  Truncates a table to those records for which a lambda predicate is true.
    ;; *******************************************************************
       vars:(rowIndex newIndex n vec)
       (if (<= recordCount 0) (return recordCount))
       ;; If the selectLambda is a number, then call truncateMid.
       (if (isNumber findLambda) (return (__truncateMid findLambda 0)))
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
    (defun updateBegin(tableName ...)
    ;; *******************************************************************
    ;; Summary:  Begins an update transaction on the table cursor.
    ;; *******************************************************************
       ;; Search for the specified table in the primary and blackboard areas.
       vars:(indexOf record)
       (__clear)
       (setq myTableCursor (myDataMine.updateBegin tableName))
       (objectToStructure myParent.Pv myTableCursor.mySchema)
       ;; If we're opening a view, we take the columns of the view's table.
       (if (= myObjectType view:)
           (begin
              (setq colCount myTableCursor.colCount)
              (setq colVector myTableCursor.colVector)
              )) ; end if
       ;; If the caller has specified columns, we use them over all else.
       (if (= (argCount) 2)
           ;; Use the caller specified columns.
           (begin
              (setq colVector (argFetch 1))
              (setq colCount myTableCursor.colCount)
              (setq validFields (objectToStructure colVector #(#void)))
              (setq colVector (refAttributes validFields))
              (setq recordStructure (new Vector: colCount))
              (setAttributes recordStructure colVector)
              (loop for indexOf from 0 until myTableCursor.recordCount do
                  (setq record (myTableCursor.updateRead indexOf))
                  (setq record (objectToVector (copy recordStructure) record))
                  (updateWrite indexOf record)
                  ) ;; end record load loop
              ) ;; end use caller specified columns
           ;; Use the table's standard columns.
           (begin
              (loop for indexOf from 0 until myTableCursor.recordCount do
                  (setq record (myTableCursor.updateRead indexOf))
                  (updateWrite indexOf record)
                  ) ;; end record load loop
              )) ;; end table columns if
       (setq myTableCursor (myDataMine.updateEnd myTableCursor)) 
       true) ;; end of updateBegin
    (defun updateEnd()
    ;; *******************************************************************
    ;; Summary:  Terminates an update transaction on the table cursor.
    ;; *******************************************************************
       (__clear)
       (setq lastOperation #void)
       (setq myObjectName #void)
       (setq mySchema #void)
       (setq myIndex #void)
       (setq myRepos #void)
       (setq myExtCount #void)
       (setq myDataMine.Pv.updateCursor[myCursorName] #void)
       true) ;; end of updateEnd
    (defun updateDrop()
    ;; *******************************************************************
    ;; Summary:  Drops all records from the table.
    ;; *******************************************************************
       (__clear)) ;; end of updateDrop
    (defun updateRead(row)
    ;; *******************************************************************
    ;; Summary:  Reads a record from the table for later update.
    ;; Note:     Reads do not have to be sequential.
    ;;           Writes do not have to be sequential.
    ;;           Each update session must be started with an updateBegin
    ;; Args:     row      Row number of row to be read for later update.
    ;; Return:   record   The row just read. 
    ;; *******************************************************************
       (if (= row #void) (setq row 0))
       (if (or (< row 0) (> row recordCount))
           (error (append "badRowIndex" 
                          "dmMemoryCursor:updateRead:" myObjectName " - " row
                          ":An attempt was made to read for update with a bad row number.")))
       rowVector[row]) ;; end of updateRead
    (defun updateSave()
    ;; *******************************************************************
    ;; Summary:  Save the contents of the table cursor back to disk.
    ;; *******************************************************************
       vars:(indexOf record)
       (if (= myObjectType view:)
           (error (append "updateSave" 
                          "dmMemoryCursor:updateSave:" myObjectName
                          ":An attempt was made to save for update into a view.")))
       ;; Clear the specified table and update the column information.
       (setq myTableCursor (myDataMine.updateBegin myObjectName))
       (myTableCursor.updateDrop)
       (setq myTableCursor.Pv.colVector colVector)
       (setq myTableCursor.Pv.colCount colCount)
       (setq myTableCursor.Pv.validFields validFields)
       (setq myTableCursor.Pv.recordStructure recordStructure)
       (setq myTableCursor.Pv.blockSize (divi myDataMine.blockSizeFactor (max colCount 1)))
       ;; Write the records in the memory cursor back to disk
       (loop for indexOf from 0 until recordCount do
           (myTableCursor.updateWrite indexOf rowVector[indexOf])
           ) ;; end record load loop
       (setq myTableCursor (myDataMine.updateEnd myTableCursor)) 
       true) ;; end of updateSave
    (defun updateWrite(row record)
    ;; *******************************************************************
    ;; Summary:  Write a record to the table for update.
    ;; Note:     Reads do not have to be sequential.
    ;;           Writes do not have to be sequential.
    ;;           Each update session must be started with an updateBegin
    ;; Args:     row      Row number of row to be read for later update.
    ;;           record   Row to be written to the table database.
    ;; Return:   record   The row just written. 
    ;; *******************************************************************
       ;; We do not allow write into views.
       (if (or (< row 0) (> row recordCount))
           (error (append "badRowIndex" 
                          "dmMemoryCursor:updateWrite:" myObjectName " - " row
                          ":An attempt was made to write for update with a bad row number.")))
       (if (= row recordCount) (setq recordCount (addi recordCount 1)))
       (setAttributes record colVector)
       (setq rowVector[row] record)
       ;; Return the record just written.
       record) ;; end of updateWrite
    ;; -------------------------------------
    ;; Private methods (not for public use).
    ;; -------------------------------------
    (defun __clear()
       (setq rowVector (new Vector: object: 0))
       (setq bckVector #void)
       (setq ckpDirectory (new Directory:))
       (setq recordCount 0)
       (setq fileID #void)
       true) ;; end of __clear
    (defun __closeTableCursor()
       (if (<> myTableCursor #void)
           (setq myTableCursor (myDataMine.updateEnd myTableCursor)))
       true) ;; end of __closeTableCursor
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
    ;; Initialize this data mine memory table cursor.
    Continue::
    (setq myDataMine dataMine)
    (setq myCursorType memory:)
    (setq showLimit 10)
    (setq myParent (myself))
    (setq myType table:)
    (setq myIndex #void)
    (setq myRepos #void)
    (setq myExtCount #void)
    (setq mySchema #void)
    (setq myObjectName #void)
    (setq myTableCursor #void)
    (setq myTableCursor #void)
    (setq ckpDirectory (new Directory:))
    (setq colCount 0)
    (__clear)
    myParent) ;; end of dmMemoryCursor

















;;**EXPORTKEY**:dataMineLib:dmMemoryCursor:compileFilter
(defChildLambda dataMineLib.dmMemoryCursor compileFilter(aSpec)
;; *******************************************************************
;;  Summary: Builds a data mine table filter Lambda from the ACL specification   
;;           passed as an argument. The ACL specification is designed
;;           to allow ease of specification.
;; Depends:  rulesLib
;; Args:     aSpec      Fixup specification language text.
;; Return:   proc       A table fixup Lambda.   
;; *******************************************************************
   pvars:((debugOn true)         ;; Display generated Lambda on switch
          filterFields            ;; Temporary Dictionary of valid field names
          fixupLambda              ;; Lambda for expression transformation
          syntaxLambda             ;; Lambda for expression syntax validation
          ;; Methods list 
          assertRules             ;; Method for defining fix up rules
          buildBody               ;; Method to build the SmartLisp expression body
          buildName               ;; Method for recognizing a Legacy field name
          cmdAll                  ;; Method for parsing the "all" command
          cmdOmit                 ;; Method for parsing the "omit" command
          cmdSort                 ;; Method for parsing the "bottom" and "top" commands
          errorStop               ;; Method for error recovery
          mergeRules              ;; Method for merging "all" criteria together
          pairToVector            ;; Method for converting a list into a vector
          replaceRules            ;; Method for recursive criteria replacement
          ) ;; end of persistent variables
   vars:(proc aCmd i j m n cmds scmd theSpec cmdVector)
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> cmdAll #void) (goto Continue:))
   ;; Initialize the inline child Lambdas.
   (defun assertRules()
       ;; We only need to run this once.
       (if (isLambda fixupLambda) (return true))
       ;; Define the expression fix up rules.
       (rulesLib)
       (setq fixupLambda (new rulesLib))
       ;; Make sure there are no duplication errors
       (if (= fixupLambda.rulesDic rulesLib.rulesDic) (error "dupRules"))
       ;; Operator name, function name, and number recognition rules
       (fixupLambda.assert $OP:(lambda(x) 
                                 vars:(s (d #{and and or or min min max max + + - - * * / / ** expt < < <= <= = = <> <> >= >= > >})) 
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $REL:(lambda(x) vars:((d #{< < <= <= = = <> <> >= >= > >})) (if (isMember x d) d[x])))
       (fixupLambda.assert $FN1:(lambda(x) 
                                 vars:(s (d #{not not sin sin cos cos tan tan log log exp exp sqrt sqrt})) 
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $FN2:(lambda(x) 
                                 vars:(s (d #{min min max max}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $NAM:(lambda(x) 
                                 vars:((d #(today))) 
                                 (if (not (isMember x d)) x))
                                 ) ;; end assert
       (fixupLambda.assert $NUM:(lambda(x) (if (isNumber x) x)))
       ;; Infix to Prefix notation production rules
       (fixupLambda.assert $PFN:(lambda(a fn x y b) 
                                   vars:(p)
                                   (setq p (list fn x y)) 
                                   (if (<> b #void) (setq p (append (list p) b))) 
                                   (if (<> a #void) (setq p (append a p))) 
                                   p))
       (fixupLambda.assert '($a* $X <$FN=$OP> $Y $b*) '(<$PFN> $a $FN $X $Y $b))
       ;; Constant folding production rules
       (fixupLambda.assert $DIV:(lambda(x) (error "compileFixup" (append "compileFixup divide by zero on: " (string x true)))))
       (fixupLambda.assert $FOLD1:(lambda(op x) vars:(f) (setq f (getGlobalValue (symbol op))) (f x)))
       (fixupLambda.assert $FOLD2:(lambda(op x y) vars:(f) (setq f (getGlobalValue (symbol op))) (f x y)))
       (fixupLambda.assert '(<$Y=$OP> <$X=$NUM> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '(<$Y=$FN1> <$X=$NUM>) '(<$FOLD1> $Y $X))
       (fixupLambda.assert '(<$Y=$FN2> <$X=$NUM> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '(** $X $Y) '(expt $X $Y))
       ;; Algebraic expression reduction rules
       (fixupLambda.assert '(/ $X $X) 1)
       (fixupLambda.assert '(+ $X 0) '$X)
       (fixupLambda.assert '(- $X 0) '$X)
       (fixupLambda.assert '(* $X 0) 0)
       (fixupLambda.assert '(/ 0 0) 0)
       (fixupLambda.assert '(/ $X 0) '(<$DIV> (/ $X 0)))
       (fixupLambda.assert '(expt $X 0) 1)
       (fixupLambda.assert '(+ 0 $X) '$X)
       (fixupLambda.assert '(* 0 $X) 0)
       (fixupLambda.assert '(/ 0 $X) 0)
       (fixupLambda.assert '(expt 0 $X) 0)
       (fixupLambda.assert '(* $X 1) '$X)
       (fixupLambda.assert '(/ $X 1) '$X)
       (fixupLambda.assert '(expt $X 1) '$X)
       (fixupLambda.assert '(* 1 $X) '$X)
       (fixupLambda.assert '(expt 1 $X) 1)
       ;; _ANY_ empty indices reduction rules
       (fixupLambda.assert '(<$O=$REL> (ref (ref (ref x $X)) $Y) $Z) 
                          '(_ANY_ ($O (ref x $Y) $Z) (ref x $X)))
       ;; One based indices reduction rules
       (fixupLambda.assert '(ref (ref x $X) <$Y=$NUM>) '(ref (ref x $X) (sub1 $Y)))
       ;; Excess parentheses reduction rules
       (fixupLambda.assert '(($X*)) '$X)
       (fixupLambda.assert '(<$X=$NAM>) '$X)
       ;; Define the expression syntax validation rules.
       (setq syntaxLambda (new rulesLib))
       (syntaxLambda.setFailure false)
       ;; Make sure there are no duplication errors
       (if (= syntaxLambda.rulesDic fixupLambda.rulesDic) (error "dupRules"))
       ;; Prefix notation recognition rules
       (syntaxLambda.assert '(ref $X $Z) false)
       (syntaxLambda.assert '(<$Y=$FN1> <$X=$NUM>) '(<$FOLD1> $Y $X))
       (syntaxLambda.assert '(<$Y=$OP> $X $Z) false)
       (syntaxLambda.assert '(<$Y=$FN1> $X) false)
       (syntaxLambda.assert '(<$Y=$FN2> $X $Z) false)
       (syntaxLambda.assert '(today) false)
       ;; _ANY_ macro substitution rules
       (syntaxLambda.assert '(_ANY_ $A $B) 
                           '(mapc (lambda(x) (let ((r r)) (if $A (setq r true)) r)) $B))
       ;; Unrecognized syntax error rules
       (syntaxLambda.assert $ERR:(lambda(x) (error "compileFixup" (append "compileFixup syntax error on: " (string x true)))))
       (syntaxLambda.assert '$ALL$ '(<$ERR> $ALL$))
       ;; Operator and function name recognition lambda rules
       (syntaxLambda.assert $OP:(lambda(x) 
                                 vars:(s (d #{and and or or min min max max + + - - * * / / expt expt < < <= <= = = <> <> >= >= > >}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FN1:(lambda(x) 
                                 vars:(s (d #{isBoolean isBoolean isNumber isNumber isDate isDate isSymbol isSymbol isString isString isDictionary isDictionary not not sin sin sub1 sub1 cos cos tan tan log log exp exp sqrt sqrt}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FN2:(lambda(x)
                                 vars:(s (d #{min min max max mapc mapc}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FOLD1:(lambda(op x) vars:(f) (setq f (getGlobalValue (symbol op))) (f x)))
       (syntaxLambda.assert $NUM:(lambda(x) (if (isNumber x) x)))
       true) ;; end assertRules
   (defun buildBody(v p begIndex endIndex)
   ;; Define the build expression body function. This function builds SmartLisp
   ;;  expression bodies.
   ;;  such as (1 {A1} true etc).
       vars:(fieldName s i n)
       (setq s "")
       (loop for i from begIndex until endIndex do
           (setq s (append s " " (buildName v[i] p)))
           ) ;; end loop
       ;; Apply expression transformation rules.
       (if (= p "x")
           (fixupLambda.setVerbose false)
           (fixupLambda.setVerbose false))
       (setq s (lisp s arithmetic:))
       (setq fixupLambda.singlePass false)
       (setq s (fixupLambda.apply s))
       (setq s (syntaxLambda.apply s))
       (setq s (string s true))
       s) ;; end buildBody
   (defun buildName(s p)
   ;; Define the build name function. This function recognizes Legacy
   ;;  field names inclosed in vertical bar | symbols, or SmarLisp constants
   ;;  such as (1 {A1} true etc).
       vars:(fieldName)
       (cond ;; Recognize a Legacy field name inclosed in vertical bar | symbols.
             ;;  Return the construct: p.|Fieldname|
             ((and (isType Symbol: s) (= s[0] #\|))
              (begin
                 (setq fieldName s)
                 (if (= (member fieldName validFields) false)
                     (error "buildName"))
                 (setq filterFields[fieldName] validFields[fieldName])
                 (append p "." fieldName)))
             ;; Recognize a Legacy field name not inclosed in vertical bar | symbols.
             ;;  Return the construct: p.|Fieldname|
             ((and (isType Symbol: s) (<> (member s validFields) false))
              (begin
                 (setq fieldName s)
                 (setq filterFields[fieldName] validFields[fieldName])
                 (append p "." fieldName)))
             ;; Recognize the today operator symbols.
             ;;  Convert the symbol to a List contant.
             ((= s today:)
              (append "(" s ")"))
             ;; Recognize all other non-arithmetic operator symbols.
             ;;  Convert the symbol to a string contant.
             ((and (isType Symbol: s) (= (member s #(isBoolean isNumber isDate isSymbol isString isDictionary and or not + - * / < > >= <= = <> ref |(| |)| |[| |]|)) false))
              (append "{" s "}"))
             ;; Repackage string constants within braces.
             ;;  Convert the string to a string contant.
             ((and (isString s) (= (member s #(isBoolean isNumber isDate isSymbol isString isDictionary and or not + - * / < > >= <= = <> ref |(| |)| |[| |]|)) false))
              (append "{" s "}"))
             ;; Recognize all quoted symbols.
             ;;  Convert the quoted symbol to a string contant.
             ((isType QuotedSymbol: s)
              (append "{" s "}"))
             ;; Assume we have a SmartLisp constant.
             ;;  Return the constant unaltered.
             (else s)
             ) ;; end cond
        ) ;; end buildName
   (defun cmdAll(parms s)
   ;; Define the "all" command parser.
   ;;   for example: "all = |Timeliness| 1"
       vars:(parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (= parmCnt 1)
           (return "")) 
       (if (< parmCnt 4)
           (error "compileFixup" s)) 
       (if (<> parms[0] "all")
           (error "compileFixup" s)) 
       (if (and (= parmCnt 4)
                (isBoolean (member parms[2] #("=" "<>" "<=" ">=" "<" ">"))))
           (error "compileFixup" s))
       ;; Build the resulting find command.
       (setq outS (append "(truncate (lambda(x) (onError (lambda(s) false)) "
                          (buildBody parms "x" 1 parmCnt) "))"))
       outS) ;; end cmdAll
   (defun cmdOmit(parms s)
   ;; Define the "all" command parser.
   ;;   for example: "omit |Timeliness| = 5"
       vars:(parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (= parmCnt 1)
           (return "")) 
       (if (< parmCnt 4)
           (error "compileFixup" s)) 
       (if (<> parms[0] "omit")
           (error "compileFixup" s)) 
       (if (and (= parmCnt 4)
                (isBoolean (member parms[2] #("=" "<>" "<=" ">=" "<" ">"))))
           (error "compileFixup" s))
       ;; Build the resulting find command.
       (setq outS (append "(truncate (lambda(x) (onError (lambda(s) false)) (not "
                          (buildBody parms "x" 1 parmCnt) ")))"))
       outS) ;; end cmdOmit
   (defun cmdSort(parms s)
   ;; Define the "top", "bottom", & "sort" command parsers.
   ;;   for example: "top |Price| 5"
       vars:(parmCnt lastParm outS n ratio sortOp)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       (setq lastParm (subi parmCnt 1))
       ;; Check command for number of arguments.
       (cond 
           ;; Manage simple truncate: "top cutoff"
           ((and (= parmCnt 2) (isMember parms[0] #(top: bottom:)))
            (begin 
               (if (= parms[0] "top")
                   (error "topInvalid")
                   (setq sortOp "<="))
               (cond ((isInteger parms[1])
                      (setq ratio (append "" parms[1])))
                     ((and (isNumber parms[1]) (< parms[1] 1))
                      (setq ratio (append "(integer (* TBC.recordCount " parms[1] "))")))
                     ((= parms[1] "all")
                      (setq ratio "TBC.recordCount"))
                     (else
                      (error "compileFixup" parms))
                   ) ;; end cond
               ;; Build the resulting find command.
               (setq outS (append "(truncate " ratio ")"))))
           ;; Manage simple field sort: "top,value1,cutoff"
           ((and (= parmCnt 3) (isMember parms[0] #(top: bottom:)))
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">=")
                   (setq sortOp "<="))
               (cond ((isInteger parms[2])
                      (setq ratio (append "" parms[2])))
                     ((and (isNumber parms[2]) (< parms[2] 1))
                      (setq ratio (append "(integer (* TBC.recordCount " parms[2] "))")))
                     ((= parms[2] "all")
                      (setq ratio "TBC.recordCount"))
                     (else
                      (error "compileFixup" parms))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(sort (lambda(x y) (onError (lambda(s) false)) (" sortOp
                                  " " (buildName parms[1] "x")
                                  " " (buildName parms[1] "y") "))" _eol "         "
                                  "(lambda(x) (onError (lambda(s) false))"
                                  " " (buildName parms[1] "x") ")" ;;  _eol "         "
                                  " |" sortOp "|:) ; end sort" _eol "   "))
               (setq outS (append outS "(truncate " ratio ")"))))
           ;; Manage simple expression sort: "top,value1,/,value2,cutoff" 
           ;; Note: (for backward compatibility).
           ((and (= parmCnt 5) 
                 (isMember parms[0] #(top: bottom:)) 
                 (isNumber (member parms[2] #("+" "-" "*" "/"))))
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">=")
                   (setq sortOp "<="))
               (cond ((isInteger parms[4])
                      (setq ratio (append "" parms[4])))
                     ((and (isNumber parms[4]) (< parms[4] 1))
                      (setq ratio (append "(integer (* TBC.recordCount " parms[4] "))")))
                     ((= parms[4] "all")
                      (setq ratio "TBC.recordCount"))
                     (else
                      (error "compileFixup" parms))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(sort (lambda(x y) (onError (lambda(s) false)) (" sortOp
                                   " (" parms[2] " " (buildName parms[1] "x") " " (buildName parms[3] "x") ")"
                                   " (" parms[2] " " (buildName parms[1] "y") " " (buildName parms[3] "y") ")"
                                   "))" _eol "         "
                                   "(lambda(x) (onError (lambda(s) false))"
                                   " (" parms[2] " " (buildName parms[1] "x") " " (buildName parms[3] "x") "))"
                                   " |" sortOp "|:) ; end sort" _eol "   "))
               (setq outS (append outS "(truncate " ratio ")"))))
           ;; Manage complex expression sort: "top,/,value1,value1,cutoff" 
           (else
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">=")
                   (setq sortOp "<="))
               (cond ((isInteger parms[lastParm])
                      (setq ratio (append "" parms[lastParm])))
                     ((and (isNumber parms[lastParm]) (< parms[lastParm] 1))
                      (setq ratio (append "(integer (* TBC.recordCount " parms[lastParm] "))")))
                     ((= parms[lastParm] "all")
                      (setq ratio "TBC.recordCount"))
                     (else
                      (error "compileFixup" parms))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(sort (lambda(x y) (onError (lambda(s) false)) (" sortOp " "
                                   (buildBody parms "x" 1 lastParm) " "
                                   (buildBody parms "y" 1 lastParm) "))"  _eol "         "
                                   "(lambda(x) (onError (lambda(s) false))"
                                   " " (buildBody parms "x" 1 lastParm) ")"
                                   " |" sortOp "|:) ; end sort" _eol "   "))
               (setq outS (append outS "(truncate " ratio ")"))))
           ) ;end cond
       outS) ;; end cmdSort
   (defun errorStop(err) (writeln "compileFixup: " err) false)
   (defun mergeRules(cmds)      
   ;; Merges consecutive "all" rules together with an "and" conjunction.
   ;; Looks for "all" rules and eliminates them.
       vars:(cmdIndex args newCmds ruleLambda i)
       (loop for cmdIndex from 0 until (length cmds) do
           (cond
              ;; Replace "all" command with nothing,
              ;; because and "all" command is a noop.
              ((= cmds[cmdIndex] "all")
               (begin
                  (setq cmds (delete cmds cmdIndex))
                  (setq cmdIndex (max (- cmdIndex 2) -1))
                  )) ;; end begin
              ;; Replace "remarks" command with nothing,
              ;; because and "remarks" command is a comment.
              ((= (left cmds[cmdIndex] 7) "remarks")
               (begin
                  (setq cmds (delete cmds cmdIndex))
                  (setq cmdIndex (max (- cmdIndex 2) -1))
                  )) ;; end begin
              ;; Replace "omit" command with "all (not )",
              ((= (left cmds[cmdIndex] 5) "omit ")
               (begin
                  (setq newCmds (append "all (not ("
                                        (mid cmds[cmdIndex] 5 100000)
                                        "))"))
                  (setq cmds[cmdIndex] newCmds)
                  (setq cmdIndex (max (- cmdIndex 2) -1))
                  )) ;; end begin
              ;; Merge consecutive "all" commands together,
              ;; with a conjunctive "and" operator.
              ((and (= (left cmds[cmdIndex] 4) "all ")
                    (< (add1 cmdIndex) (length cmds))
                    (= (left cmds[(add1 cmdIndex)] 4) "all "))
               (begin
                  (setq newCmds (append "all ("
                                        (mid cmds[cmdIndex] 4 100000)
                                        ") and ("
                                        (mid cmds[(add1 cmdIndex)] 4 100000)
                                        ")"))
                  (setq cmds (delete cmds cmdIndex))
                  (setq cmds[cmdIndex] newCmds)
                  (setq cmdIndex (max (sub1 cmdIndex) -1))
                  )) ;; end begin
               ) ;; end cond
           ) ;; end of loop
       cmds) ;; end of mergeRules
   (defun pairToVector(v p)
       vars:(i n result)
       (if (= v #void) (setq v (new Vector: 0)))
       (setq n (length p))
       (loop for i from 0 until n do
          (if (isAtom p[i])
              (setq v[(length v)] p[i])
              (begin
                 (setq v[(length v)] |(|:)
                 (setq v (pairToVector v p[i]))
                 (setq v[(length v)] |)|:)
                 )) ;; end if
          ) ;; end loop
       v) ;; end pairToVector
   (defun replaceRules(cmds)
   ;; Recursively replaces rules with their command strings.
   ;; Looks for "fixup" rules in the parent data mine Lambda.
       vars:(cmdIndex args newCmds i)
       (loop for cmdIndex from 0 until (length cmds) do
           ;; Check for rule command
           (if (= (left cmds[cmdIndex] 8) "fixup")
               (begin
                  (setq args (objectToVector (lisp cmds[cmdIndex] arithmetic:)))
                  ;; Retrieve the named fix up Lambda source
                  ;; and covert to a vector of commands
                  (if (and (= (left args[1] 1) "{") (= (right args[1] 1) "}")) 
                      (setq args[1] (mid args[1] 1 (subi (length args[1]) 2))))
                  (setq newCmds (myDataMine.__getFixupSource args[1]))
                  (setq newCmds (stringToVector newCmds ";"))
                  ;; Replace substitute rules within the original 
                  ;; vector of command strings.
                  (setq cmds[cmdIndex] newCmds[0])
                  (loop for i from 1 until (length newCmds) do
                      (setq cmds (vectorInsert cmds (addi cmdIndex i) newCmds[i]))
                      ) ;; end loop
                  ;; Make sure we retest the first substituted rule.
                  (-- cmdIndex)
                  )) ;; end if
           ) ;; end of loop
       cmds) ;; end of replaceRules
   ;; Initialize the Lambda.
   Continue::
   (onError errorStop)
   (setq filterFields (new Dictionary:))
   (assertRules)
   ;; Break the command up into its sub-commands.
   (setq cmds (stringToVector aSpec ";"))
   ;; Perform recursive rule substitution.
   (setq cmds (replaceRules cmds)) 
   ;; Establish the filter names array.
   (loop for i from 0 until (length cmds) do
       ;; Establish the filter names for each command.
       (setq cmdVector (pairToVector #void (lisp cmds[i] arithmetic:)))
       (loop for j from 0 until (length cmdVector) do
           (buildName cmdVector[j] "x")
           ) ;; end j loop
       ) ;end i loop
   ;; Add the filter names array as additional restrictions (if "nocheck" command not present).
   (if (and (> (length cmds) 0) (<> (trim cmds[0]) "nocheck"))
       (begin
          (loop for i from 0 until (length filterFields) do
             (if (<> filterFields[i 1] isDictionary:)
                 (begin
                    (if (<> filterFields[i 1] #void)
                        (begin
                           (setq aCmd (append "all (" filterFields[i 1] " |" filterFields[i 0] "|)"))
                           (setq cmds (vectorInsert cmds 0 aCmd))
                           ) ; end field type is not #void
                        (begin
                           (setq aCmd (append "all (<> " filterFields[i 0] " #void)"))
                           (setq cmds (vectorInsert cmds 0 aCmd))
                           )) ; end field type is #void
                    ))) ;; end loop
          )) ; end if
   ;; Perform rule merging.
   (setq cmds (mergeRules cmds))
   ;; Parse each sub command and append to the specification.
   (setq theSpec "")
   (loop for i from 0 until (length cmds) do
       ;; Call the proper command parser (based on the command word).
       (setq cmdVector (pairToVector #void (lisp cmds[i] arithmetic:)))
       (cond 
           ((= cmdVector[0] "all") (setq scmd (cmdAll cmdVector cmds[i]))) 
           ((= cmdVector[0] "check") (setq scmd ""))
           ((= cmdVector[0] "nocheck") (setq scmd ""))
           ((= cmdVector[0] "bottom") (setq scmd (cmdSort cmdVector cmds[i])))
           ((= cmdVector[0] "omit") (setq scmd (cmdOmit cmdVector cmds[i]))) 
           ((= cmdVector[0] "top") (setq scmd (cmdSort cmdVector cmds[i])))
           (else (error "compileFixup" cmds[i]))
           ) ; end cond
       (setq theSpec (append theSpec scmd _eol "   "))
       ) ;end loop
   ;; Convert the screen specification into a procedure.
   (setq proc (append "(lambda(TBC)" _eol "   "))
   (setq proc (append proc ";; TBC must be a data mine table cursor object." _eol "   "))
   (setq proc (append proc "vars:(restore sort truncate)" _eol "   "))
   (setq proc (append proc "(setq truncate TBC.truncate)" _eol "   "))
   (setq proc (append proc "(setq restore TBC.restore)" _eol "   "))
   (setq proc (append proc "(setq sort TBC.sort)" _eol "   "))
   (if (<> theSpec "")
       (setq proc (append proc theSpec)))
   (setq proc (append proc "true)"))
   (if (= debugOn true) (writeln proc)) ;; Use for testing only
   (setq proc (eval proc))
   (if (not (isLambda proc)) (error "compileFixup"))
   proc) ;; end compileFilter

;;  ***NOTES***:
;; 
;; see the screenBuilder notes, this Lambda implements the same query language.























;;**EXPORTKEY**:dataMineLib:dmTableCursor
(defriend dataMineLib:dmTableCursor(dataMine)
;; *******************************************************************
;; Summary:  Data Mine table cursor which manages a table in
;;           the data mine schema. This table cursor is built 
;;           on principles similar to relational cursors.
;; Notes:    Each element in the table is called a record.
;;           The index of a table record is called its row (rows begin with zero).
;; Depends:  dataMineLib
;; args:     dataMine	    The data mine Lambda owning this cursor.
;; Return:   self           This newly initialized table cursor.
;; *******************************************************************
    ;; Persistent Variables
    pvars:(fileID              ;; The file id used during import and export.
           myCursorName        ;; The unique name of this data mine table cursor.
           myCursorType        ;; The type of this data mine table cursor (disk).
           myDataMine          ;; The data mine owner of this cursor.
           myExtCount          ;; Number of repository extents.
           myIndex             ;; The schema repository for data mine tables.
           myParent            ;; This parent cursor Lambda.
           myRepos             ;; This vector of data mine table repositories.
           myTableCursor       ;; The cursor for the table of this view (views only).
           myType              ;; The type of this data mine cursor.
           newBlockNUM         ;; The number of the new block.
           newBlockEXT         ;; The extent number for the new block.
           newBlockIDX         ;; The record index within the new block.
           newBlockKEY         ;; The retrieval key for the new block.
           showLimit           ;; The maximum number of records to display with the show function.
           ;; Schema globals (see dataMineLib table schema).
           ;; Note: We load the schema into our Pv to speed record processing.
           ;;       We name blocks by root name and block number. Block name key roots 
           ;;      (for current, temporary, backup, and checkpoint blocks) are stored here.
           mySchema            ;; The table object schema record (#void if cursor is inactive).
            bckBlockRoot       ;; The retrieval key (root name only) for all backup blocks.
            bckSchema          ;; The table's backup schema (#void if no backup).
            blockCount         ;; The table's current block count.
            blockSize          ;; The number of records per table block.
            ckpBlockRoot       ;; The retrieval key (root name only) for all checkpoint blocks.
            ckpSchema          ;; The table's checkpoint schema (#void if no checkpoint).
            colCount           ;; The number of columns in the table.
            colVector          ;; The table's vector of column names.
            myObjectName       ;; The name of the data mine table or view object (#void if cursor is inactive).
            myObjectType       ;; The name of the data mine object type (#void if cursor is inactive).
            myTableName        ;; The name of the table for this view (#void if this is a table object).
            myTableDate        ;; The last update date of the table for this view (#void if this is a table object).
            newBlockRoot       ;; The retrieval key (root name only) for all new blocks.
            recordCount        ;; The table's current total record count.
            recordStructure    ;; The table's record structure.
            rowOrderSW         ;; The view is sorted by row order (views only).
            tmpBlockRoot       ;; The retrieval key (root name only) for all temporary block.
            validFields        ;; The table's valid fields and types (necessary for query compilation).
            viewFilter         ;; The view's initial filter (necessary for view resynching).
           ;; Update transaction globals (see updateBegin and updateEnd).
           UPDcurrentBlock     ;; The current block of table records.
           UPDblockNUM         ;; The number of the current block.
           UPDblockEXT         ;; The extent number for the current block.
           UPDblockIDX         ;; The record index within the current block.
           UPDblockKEY         ;; The retrieval key for the current block.
           UPDIamDirty         ;; Update has occurred switch.
           UPDInProgress       ;; Update transaction in progress switch.
           ;; Public Child Lambdas
           backup              ;; Creates a backup copy of the data mine table.
           checkPoint          ;; Creates a check point copy of the data mine table.
           compileFilter       ;; Returns a compiled filter Lambda from the specified ACL source.
           delimitedCells      ;; Returns a block of cells as a tab delimited string.
           exportTab           ;; Exports ascii tab delimited records from the table.
           getColumnHeaders    ;; Returns the table headers as a tab delimited string.
           importTab           ;; Imports ascii tab delimited records into the table.
           refExport           ;; Returns a record, for export, to the exportTab function.
           refImport           ;; Returns an empty record to the importTab function.
           restore             ;; Restores the backup copy of the data mine table.
           rollBack            ;; Restores the check point copy of the data mine table.
           setImport           ;; Receives a single record from the importTab function.
           show                ;; Shows a group of records starting from the specified row.
           sort                ;; Sort the table rows using a lambda sort function.
           truncate            ;; Truncates a table to those records for which a lambda predicate is true.
           updateBegin         ;; Begin an update transaction on a table object.
           updateDrop          ;; Drop all records from a table object.
           updateEnd           ;; Terminate an update transaction on the table object.
           updateRead          ;; Read a table row for later update.
           updateSave          ;; Save the contents of the table cursor back to disk.
           updateWrite         ;; Write a table row, perhaps previously read, for update.
           ;; Private Child Lambdas
           __clear             ;; Clears the update global variables.
           __closeTableCursor  ;; Closes the table cursor (views only).
           __cnvBlockExt       ;; Returns the block extent from the record number.
           __cnvBlockIdx       ;; Returns the block record index from the record number.
           __cnvBlockInfo      ;; Sets the new block global variables from the record number.
           __cnvBlockKey       ;; Returns the block retrieval key from the record number.
           __cnvBlockNum       ;; Returns the block number from the record number.
           __copyBlocks        ;; Copies the specified table record blocks.
           __deleteBlocks      ;; Delete the specified table record blocks.
           __dropAllRecords    ;; Delete all record data and information.
           __dropBackup        ;; Delete all backup data and information.
           __dropCheckpoint    ;; Delete all checkpoint data and information.
           __dropTemporary     ;; Delete all temporary data and information.
           __errorStop         ;; Handles error conditions during sensitive operations.
           __makeBlockList     ;; Make a list of temporary block numbers.
           __merge2BlockLists  ;; Merge two lists of temporary blocks into a sequential run of new blocks.
           __merge4BlockLists  ;; Merge four lists of temporary blocks into a sequential run of new blocks.
           __mergeMBlockLists  ;; Merge multiple lists of temporary blocks into a sequential run of new blocks. 
           __readBlock         ;; Read (possibly destructively) the specified block. 
           __readBlockList     ;; Read the next record from the specified list of temporary blocks.
           __renameBlocks      ;; Renames the specified table record blocks.
           __renameTmpBlocks   ;; Renames the current blocks as temporary blocks.
           __resetRecords      ;; Reset the record and block counts to zero.
           __resynchSort       ;; Resynchronizes the view from the specified table records prior to a sort.
           __resynchView       ;; Resynchronizes the view from the specified table records.
           __sortByRow         ;; Sort the view rows by table row number (views only).
           __truncateEnds      ;; Truncate records from both ends of a table.
           __truncateMid       ;; Truncate records from the center of a table.
           __updateEnd         ;; Make sure any dirty blocks are written to the table. 
           __updateKill        ;; Terminate an update transaction and delete the table object.
           __updateRead        ;; Read a table row for later update (internal use only).
           __updateWrite       ;; Write a table row, perhaps previously read, for update (internal use only).
           __writeBlock        ;; Write the specified block. 
           ) ;; end of pvars
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> updateBegin #void) (goto Continue:))
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
       (if (<= recordCount startRow) (return dls))
       (loop for rowIndex from startRow to endRow do
          (setq dls (append dls rowIndex (char 09)))
          (setq record (updateRead rowIndex))
          (loop for colIndex from startCol until endCol do
              (setq cellValue record[colIndex])
              (if (isString cellValue) (setq cellValue (substitute cellValue tab " ")))
              (if (= cellValue #void) (setq cellValue ""))
              (if (isVector cellValue) (setq cellValue (string cellValue true)))
              (setq cellValue (append "" cellValue)) 
              (setq cellValue (substitute cellValue (char 09)  " "))
              (setq cellValue (substitute cellValue (char 13)  " "))
              (setq dls (append dls cellValue (char 09)))
              ) ;;end column loop
          (setq dls (append dls (char 10) (char 13)))
          ) ;;end row loop
       (append dls "")) ;; end of delimitedCells
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
       (loop for colIndex from startCol until endCol do
           (setq colName (string colVector[colIndex]))
           (if (= (left colName 1) "|") (setq colName (mid colName 1 (subi (length colName) 2))))
           (setq dls (append dls colName (char 09)))
           ) ;;end column loop
       (append dls "")) ;; end getColumnHeaders
    (defun importTab(fileName)
    ;; *******************************************************************
    ;; Summary:  Performs an import tab transaction on the table cursor.
    ;; *******************************************************************
       ;; We do not allow import into views.
       (if (= myObjectType view:) (error (append "importTab: An attempt was made to import into a view: " myObjectName)))
       ;; Make sure we catch all errors which might occur.
       (onError __errorStop)
       ;; Open the specified .tab file and import.
       (setq fileID (fileOpen fileName 0 0))
       (^importTab fileID myParent recordVectors:)
       (fileClose fileID 1)
       (setq fileID #void)
       (if UPDIamDirty
          (begin
             (setq myRepos[UPDblockEXT][UPDblockKEY] (setAttributes UPDcurrentBlock #void))
             (setq myIndex[myObjectName] (objectToStructure mySchema myParent.Pv))
          )) ;; end if
       (__clear)
       recordCount) ;; end importTab
    (defun refExport(row)
    ;; *******************************************************************
    ;; Summary:  Returns a record, for export, to the exportTab function.
    ;; *******************************************************************
       ;; If the row is zero, return the column names for the header record.
       (if (= row 0) 
           (if (<> myTableCursor #void)
               (return myTableCursor.colVector)
               (return colVector)
               ) ; end if
           ) ; end if
       ;; Adjust the record index for the header record.
       (setq row (subi row 1))
       ;; If there are no more records, tell exportTab to stop exporting.
       (if (>= row recordCount) (return false))
       ;; Otherwise return the next record for export.
       (updateRead row)) ;; end of refExport 
    (defun refImport(row)
    ;; *******************************************************************
    ;; Summary:  Returns an empty record to the importTab function.
    ;; *******************************************************************
       ;; If the row is zero, return an empty vector to hold the column names,
       ;; Otherwise, return an empty record (use the recordStructure as a template).
       (if (= row 0)
           (return (^new Vector: 0))
           (return (copy recordStructure)))) ;; end of refImport
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
                  (setq colCount (length colVector))
                  (setq recordStructure (^new Vector: colCount))
                  (setAttributes recordStructure colVector)
                  (setq blockSize (divi myDataMine.blockSizeFactor (max colCount 1)))
                  (return true)) ;; end setting new column names.
               (begin 
                  (setq recordVector (objectToStructure recordVector #(#void)))
                  (setq recordVector (refAttributes recordVector))
                  (if (<> colVector recordVector)
                      (error (append "importTab: An attempt was made to import a file with mismatched column names: " myObjectName)))
                  (return true)) ;; end checking new column names against old column names.
           )) ;; end if
       ;; Import all other .tab records at the end of the table.
       (__updateWrite recordCount recordVector)
       true) ;; end of setImport
    (defun show(startIndex) 
    ;; *******************************************************************
    ;; Summary:  Shows a group of records starting from the specified row.
    ;; *******************************************************************
       vars:(i n) 
       (setq n (min recordCount (addi startIndex showLimit)))
       (loop for i from startIndex until n do
           (writeln "[" i "] " (updateRead i))
           ) ;; end loop
       true) ;; end show
    (defun updateBegin(tableName)
    ;; *******************************************************************
    ;; Summary:  Begins an update transaction on the table cursor.
    ;; *******************************************************************
       ;; Search for the specified table in the primary and blackboard areas.
       (if (<> (setq mySchema myDataMine.primarySchema[tableName]) #void)
           ;; Does the primary area contain the specified table?
           (begin
              (setq myIndex myDataMine.primarySchema)
              (setq myRepos myDataMine.primaryRep)
              (setq myExtCount myDataMine.primaryExtCount))
       ;; Does the blackboard area contain the specified table?
           (begin
              (setq mySchema myDataMine.blackboardSchema[tableName])
              (setq myIndex myDataMine.blackboardSchema)
              (setq myRepos myDataMine.blackboardRep)
              (setq myExtCount myDataMine.blackboardExtCount)
           )) ;; end if
       ;; Is the table not to be found?
       (if (= mySchema #void) 
           (error (append "accessDenied"
                          "dmTableCursor:updateBegin:"
                          "An attempt was made to begin update on an unknown table.")))
       (objectToStructure myParent.Pv mySchema)
       (setq lastOperation "updateBegin")
       (__clear)
       ;; If this is a view, then resynchronize the view with the table.
       (if (= myObjectType view:) (__resynchView))
       true) ;; end of updateBegin
    (defun updateDrop()
    ;; *******************************************************************
    ;; Summary:  Drops all records from the table.
    ;; *******************************************************************
       vars:(extIndex blkIndex anExtent name)
       ;; Drop each block from the repository extents.
       (if (<> bckBlockRoot #void) (__dropBackup bckSchema.blockCount))
       (if (<> ckpBlockRoot #void) (__dropCheckpoint ckpSchema.blockCount))
       (if (<> tmpBlockRoot #void) (__dropTemporary blockCount))
       (loop for extIndex from 0 until myExtCount do
          (setq anExtent myRepos[extIndex])
          (beginTransaction anExtent)
          (setq blkIndex extIndex)
          (while (<= blkIndex blockCount) do
             (setq name (append newBlockRoot "_" blkIndex))
             (setq anExtent[name] #void)
             (setq blkIndex (addi blkIndex myExtCount))
             ) ;; end while
          (commitTransaction anExtent)
          ) ;; end loop
       (setq lastOperation "updateDrop")
       (setq blockCount 0)
       (setq recordCount 0)
       (setq myIndex[myObjectName] (objectToStructure mySchema myParent.Pv))
       (__clear)) ;; end of updateDrop
    (defun updateEnd()
    ;; *******************************************************************
    ;; Summary:  Terminates an update transaction on the table cursor.
    ;; *******************************************************************
       ;; Save current block and update the table schema record?
       (if UPDIamDirty
          (begin
             (setq myRepos[UPDblockEXT][UPDblockKEY] (setAttributes UPDcurrentBlock #void))
             (setq myIndex[myObjectName] (objectToStructure mySchema myParent.Pv))
          )) ;; end if
       (setq lastOperation #void)
       (__clear)
       (setq myObjectName #void)
       (setq mySchema #void)
       (setq myIndex #void)
       (setq myRepos #void)
       (setq myExtCount #void)
       (setq myDataMine.Pv.emptyCursor[myCursorName] myParent)
       (setq myDataMine.Pv.updateCursor[myCursorName] #void)
       true) ;; end of updateEnd
    (defun updateRead(row)
    ;; *******************************************************************
    ;; Summary:  Reads a record from the table for later update.
    ;; Note:     Reads do not have to be sequential.
    ;;           Writes do not have to be sequential.
    ;;           Each update session must be started with an updateBegin
    ;; Args:     row      Row number of row to be read for later update.
    ;; Return:   record   The row just read. 
    ;; *******************************************************************
       (if (= row #void) (setq row 0))
       (if (or (< row 0) (> row recordCount))
           (error (append "badRowIndex" 
                          "dmTableCursor:updateRead:" myObjectName 
                          ":An attempt was made to read for update with a bad row index.")))
       ;; Compute new block and record numbers.
       (__cnvBlockInfo row)
       ;; Need different block of records?
       (if (<> newBlockNUM UPDblockNUM)
           (begin
              ;; Save current block and update the table schema record?
              (if UPDIamDirty
                 (begin
                    (setq myRepos[UPDblockEXT][UPDblockKEY] (setAttributes UPDcurrentBlock #void))
                    (setq myIndex[myObjectName] (objectToStructure mySchema myParent.Pv))
                    )) ;; end if
              (setq UPDIamDirty false)
              (setq UPDblockKEY newBlockKEY)
              (setq UPDblockEXT newBlockEXT)
              (setq UPDblockNUM newBlockNUM)
              (setq UPDcurrentBlock myRepos[UPDblockEXT][UPDblockKEY])
              (if (= UPDcurrentBlock #void) (setq UPDcurrentBlock (new Vector: object: 0)))
              (setAttributes UPDcurrentBlock colVector)
              )) ;; end if
       ;; Return the requested record for later update.
       (setq UPDblockIDX newBlockIDX)
       ;; If this is a view, read the table indirect; otherwise return the record.
       (if (<> myTableCursor #void)
           (myTableCursor.updateRead UPDcurrentBlock[UPDblockIDX].__row)
           UPDcurrentBlock[UPDblockIDX]
           )) ;; end of updateRead
    (defun updateSave()
    ;; *******************************************************************
    ;; Summary:  Save the contents of the table cursor back to disk.
    ;; *******************************************************************
       (if (= myObjectType view:)
           (error (append "updateSave" 
                          "dmMemoryCursor:updateSave:" myObjectName
                          ":An attempt was made to save for update into a view.")))
       (__updateEnd)     
       true) ;; end of updateSave
    (defun updateWrite(row record)
    ;; *******************************************************************
    ;; Summary:  Write a record to the table for update.
    ;; Note:     Reads do not have to be sequential.
    ;;           Writes do not have to be sequential.
    ;;           Each update session must be started with an updateBegin
    ;; Args:     row      Row number of row to be read for later update.
    ;;           record   Row to be written to the table database.
    ;; Return:   record   The row just written. 
    ;; *******************************************************************
       ;; We do not allow write into views.
       (if (= myObjectType view:) (error (append "updateWrite" "An attempt was made to write into a view: " myObjectName)))
       (__updateRead row)
       (setq UPDIamDirty true)
       (setq lastOperation "updateWrite")
       ;; Update the record to the current block.
       (setAttributes record colVector)
       (setq UPDcurrentBlock[UPDblockIDX] record)
       (if (>= row recordCount) (setq recordCount (add1 row)))
       (if (>= UPDblockNUM blockCount) (setq blockCount (add1 UPDblockNUM)))
       ;; Return the record just written.
       record) ;; end of updateWrite
    ;; Initialize this data mine table cursor.
    Continue::
    (setq myDataMine dataMine)
    (setq myCursorType disk:)
    (setq showLimit 10)
    (setq myParent (myself))
    (setq myType table:)
    (setq myIndex #void)
    (setq myRepos #void)
    (setq myExtCount #void)
    (setq mySchema #void)
    (setq myObjectName #void)
    (setq myTableCursor #void)
    (__clear)
    myParent) ;; end of dmTableCursor























;;**EXPORTKEY**:dataMineLib:dmTableCursor:__backupMethods
(defChildLambda dataMineLib.dmTableCursor __backupMethods()
;; *******************************************************************
;; Summary:  Data Mine table cursor private child Lambdas.
;; Depends:  dataMineLib
;;           dmTableCursor
;; args:     errMsg	    The error message for this cursor.
;; Return:   error          This newly created table cursor error.
;; *******************************************************************
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> __clear #void) (goto Continue:))
    ;; Initialize private child Lambda methods.
    (defun __copyBlocks(toBlockRoot fromBlockRoot begCount maxCount)
    ;; *******************************************************************
    ;; Summary:  Copies the specified record blocks of the table.
    ;; *******************************************************************
       vars:(extIndex blkIndex anExtent toName fromName)
       ;; Drop each block from the repository extents.
       (loop for extIndex from 0 until myExtCount do
          (setq anExtent myRepos[extIndex])
          (beginTransaction anExtent)
          (setq blkIndex extIndex)
          (while (<= blkIndex maxCount) do
             (if (>= blkIndex begCount)
                 (begin
                    (setq toName (append toBlockRoot "_" blkIndex))
                    (setq fromName (append fromBlockRoot "_" blkIndex))
                    (setq anExtent[toName] anExtent[fromName])
                 )) ;; end if
             (setq blkIndex (addi blkIndex myExtCount))
             ) ;; end while
          (commitTransaction anExtent)
          ) ;; end loop
       true) ;; __copyBlocks
    (defun __deleteBlocks(theBlockRoot begCount endCount)
    ;; *******************************************************************
    ;; Summary:  Deletes the specified record blocks of the table.
    ;; *******************************************************************
       vars:(extIndex blkIndex anExtent name)
       ;; Drop each block from the repository extents.
       (loop for extIndex from 0 until myExtCount do
          (setq anExtent myRepos[extIndex])
          (beginTransaction anExtent)
          (setq blkIndex extIndex)
          (while (<= blkIndex endCount) do
             (if (>= blkIndex begCount)
                 (begin
                    (setq name (append theBlockRoot "_" blkIndex))
                    (setq anExtent[name] #void)
                 )) ;; end if
             (setq blkIndex (addi blkIndex myExtCount))
             ) ;; end while
          (commitTransaction anExtent)
          ) ;; end loop
       true) ;; __deleteBlocks
    (defun __dropAllRecords()
    ;; *******************************************************************
    ;; Summary:  Drops all records from the table.
    ;; *******************************************************************
       vars:(extIndex blkIndex anExtent name)
       (__deleteBlocks newBlockRoot 0 blockCount)
       (setq blockCount 0)
       (setq recordCount 0)
       (setq myIndex[myObjectName] (objectToStructure mySchema myParent.Pv))
       (__clear)) ;; end of __dropAllRecords
    (defun __dropBackup(maxCount)
    ;; *******************************************************************
    ;; Summary:  Drops all backup records from the table.
    ;; *******************************************************************
       (__deleteBlocks bckBlockRoot 0 maxCount)
       (setq bckBlockRoot #void)
       (setq bckSchema #void)
       true) ;; __dropBackup
    (defun __dropCheckpoint(maxCount)
    ;; *******************************************************************
    ;; Summary:  Drops all checkpoint records from the table.
    ;; *******************************************************************
       (__deleteBlocks ckpBlockRoot 0 maxCount)
       (setq ckpBlockRoot #void)
       (setq ckpSchema #void)
       true) ;; __dropCheckpoint
    (defun __dropTemporary(maxCount)
    ;; *******************************************************************
    ;; Summary:  Drops all checkpoint records from the table.
    ;; *******************************************************************
       (__deleteBlocks tmpBlockRoot 0 maxCount)
       (setq tmpBlockRoot #void)
       true) ;; __dropTemporary
    (defun __renameBlocks(toBlockRoot toStartIndex fromBlockRoot begCount maxCount)
    ;; *******************************************************************
    ;; Summary:  Renames the specified record blocks of the table.
    ;; *******************************************************************
       vars:(extIndex anExtent toName toIndex fromName fromIndex)
       ;; Rename each block from the repository extents.
       (loop for extIndex from 0 until myExtCount do
          (setq anExtent myRepos[extIndex])
          (beginTransaction anExtent)
          (setq fromIndex extIndex)
          (while (< fromIndex maxCount) do
             (if (>= fromIndex begCount)
                 (begin
                    (setq toIndex (addi toStartIndex (subi fromIndex begCount)))
                    (setq toName (append toBlockRoot "_" toIndex))
                    (setq fromName (append fromBlockRoot "_" fromIndex))
                    (rename anExtent fromName toName)
                 )) ;; end if
             (setq fromIndex (addi fromIndex myExtCount))
             ) ;; end while
          (commitTransaction anExtent)
          ) ;; end loop
       true) ;; __renameBlocks
    (defun __resetRecords()
    ;; *******************************************************************
    ;; Summary: Reset the table record and block count to zero.
    ;; *******************************************************************
       (setq blockCount 0)
       (setq recordCount 0)
       true) ;; end of __resetRecords
    (defun __resynchSort(valueLambda sortOp)
    ;; *******************************************************************
    ;; Summary:  Resynchronizes the view with the table prior to a sort.
    ;; *******************************************************************
       vars:(tableDate tableRecordCount indexOf index blockList sortLambda)
       ;; Resynch the view with the table and sort by row number.
       (if (<> myObjectType view:) (return false))
       (__resynchView)
       ;; Read each of the old records and extract the sort value
       ;; the valueLambda selects. The view records are written
       ;; back to the view, with the extracted values.
       (if (<> rowOrderSW true) (__sortByRow))
       (setq blockList (__makeBlockList 0 blockCount blockCount))
       (__renameTmpBlocks)
       (__resetRecords)
       (setq index (__readBlockList blockList))
       (while (<> index #void)
           (setq index.__value (valueLambda (myTableCursor.updateRead index.__row)))
           (__updateWrite recordCount index)
           (setq index (__readBlockList blockList))
           ) ;; end while
       (setq myTableDate (date (inspect myTableCursor.myIndex date: myTableCursor.myObjectName)))
       (__updateEnd)
       ;; Create a new sortLambda using the sortOp specified.
       (setq sortLambda (append "(lambda(x y) (" sortOp " x.__value y.__value))"))
       (setq sortLambda (eval sortLambda))
       sortLambda) ;; ____resynchSort
    (defun __resynchView()
    ;; *******************************************************************
    ;; Summary:  Resynchronizes the view with the specified table records.
    ;; *******************************************************************
       vars:(tableDate tableRecordCount indexOf record)
       ;; If this is not a view, then return immediately.
       (if (<> myObjectType view:) (return false))
       ;; Open the view's table cursor (if necessary).
       (if (= myTableCursor #void)
           (setq myTableCursor (myDataMine.__updateBegin myTableName))
           ) ;; end open table cursor if
       ;; Get the table last changed date.
       (if (<> myTableCursor #void) 
           (setq tableDate (date (inspect myTableCursor.myIndex date: myTableCursor.myObjectName)))
           ) ;; end retrieve table last change date if
       ;; If the table does not exist or has been changed, empty the view of all
       ;; records, and fill the view with the indices of each record in the table.
       (if (or (= myTableCursor #void) (<> myTableDate tableDate))
           (begin
              ;; Drop all current view contents and reset table date.
              (updateDrop)
              (setq myTableDate tableDate)
              (setq rowOrderSW true)
              ;; Fill the view with the indices of each record in the table.
              (if (<> myTableCursor #void) (myTableCursor.__updateEnd))
              (setq tableRecordCount myTableCursor.recordCount)
              ;; Is there a view filter present?
              (if (= viewFilter #void)
                  ;; Fill the view with each record in the table.
                  (loop for indexOf from 0 until tableRecordCount do
                      (__updateWrite indexOf (new Structure: __row: indexOf __value: #void))
                      ) ;; end view populate loop
                  ;; Fill the view with only filtered records from the table.
                  (loop for indexOf from 0 until tableRecordCount do
                      (setq record (myTableCursor.updateRead indexOf))
                      (if (viewFilter record)
                          (__updateWrite indexOf (new Structure: __row: indexOf __value: #void)))
                      )) ;; end view populate loop
              (__updateEnd)
              )) ;; end table does not exits if
       true) ;; ____resynchView
    (defun __updateKill()
    ;; *******************************************************************
    ;; Summary:  Terminate an update transaction and delete the table object.
    ;; *******************************************************************
       ;; Drop all of the table records.
       (updateDrop)
       ;; Drop the table from the schema.
       (setq myIndex[myObjectName] #void)
       true) ;; end of __updateKill
    ;; Initialize this data mine table cursor.
    Continue::
    true) ;; end __backupMethods
















;;**EXPORTKEY**:dataMineLib:dmTableCursor:__mergeMBlockLists
(defChildLambda dataMineLib.dmTableCursor __mergeMBlockLists(sortLambda ...)
;; *******************************************************************
;; summary:  Merge multiple lists of blocks into a sequential run of blocks. 
;;           Each blockList is a vector of block numbers. Each blockList
;;           represents a sequential run of block numbers, and the numbers
;;           in the first blockList are immediately contiguous to the second 
;;           blockList, etc. The records in the first blockList are presorted. 
;;           The records in the second blockList are presorted, etc. When 
;;           the merge is complete, the records will be sorted across all 
;;           the blockLists.
;; Args:     sortLambda      Sort lambda value for record pairs.
;;           blockList1     A vector of block numbers to be merged.
;;              ...   
;;           blockListN     A vector of block numbers to be merged.
;; Note:     If there are only two arguments, then the second argument
;;           is a vector of block lists.
;; Return:   true        
;; *******************************************************************
    vars:(blockIndex           ;; Index of block lists
          blockListOut         ;; Block list for merged output
          outBlock             ;; Block for merged output
          outExtNum            ;; Block extent number for merged output
          outKey               ;; Block key for merged output
          outNum               ;; Block number for merged output
          passCount            ;; Number of merge passes
          records              ;; Record vector for merge compares
          blockLists           ;; Block list vector for merge compares
          continuePass         ;; True if we are to continue the merge passes
          bestIndex            ;; Index of the best record (comparison survivor)
          ) ;; end temporary variables
    ;; Capture all the block lists passed as arguments.
    ;; Note: If there are only two arguments, then the second
    ;;       argument is a vector of block lists.
    (setq passCount (sub1 (argCount)))
    (if (= passCount 1)
        ;; All block lists passes a one argument.
        (begin
           (setq blockLists (argFetch 1))
           (setq passCount (length blockLists))
           ) ;; end all block lists passed as one argument
        ;; Each block list passed as a separate argument.
        (begin
           (setq blockLists (new Vector: passCount))
           (loop for blockIndex from 0 until passCount do
              (setq blockLists[blockIndex] (argFetch (integer (add1 blockIndex))))
              ))) ;; end capture arguments loop
    ;; Initialize all aggregate storage.
    (setq records (new Vector: passCount))
    (setq blockListOut (new Vector: 0))
    (setq outBlock (new Vector: object: 0))
    (setq continuePass true)
    ;; Void all empty block lists passed as arguments.
    (loop for blockIndex from 0 until passCount do
       (if (<= (length blockLists[blockIndex]) 0) (setq blockLists[blockIndex] #void))
       ) ;; end capture arguments loop
    ;; Note: We create a list of new output block numbers,
    ;;       the input blocks have been renamed temporary so
    ;;       that we will not overwrite them with our output.
    (loop for blockIndex from 0 until passCount do
       (if (<> blockLists[blockIndex] #void) (setq blockListOut (append blockListOut blockLists[blockIndex])))
       ) ;; end output block list construction loop
    ;; Begin the merge by reading the first records.
    (loop for blockIndex from 0 until passCount do
       (setq records[blockIndex] (__readBlockList blockLists[blockIndex]))
       ) ;; end record read loop
    ;; Merge the records in both block lists together
    ;; into a single contiguous presorted run of blocks.
    (while continuePass do
        ;; Determine the best record from the block lists by comparison survival.
        (setq bestIndex 0)
        (loop for blockIndex from 1 until passCount do
           (cond
              ;; Manage the case where the second list is empty.
              ((= records[bestIndex] #void)
               (begin
                  (setq bestIndex blockIndex)
                  )) ;; end best index is empty.
              ;; Manage the case where the first list is empty.
              ((and (<> records[blockIndex] #void) (sortLambda records[blockIndex] records[bestIndex]))
               (begin
                  (setq bestIndex blockIndex)
                  )) ;; end best index is replaced.
               ) ;; end cond
            ) ;; end comparison survival loop
        ;; If the best record is void, then we are done.
        (if (= records[bestIndex] #void) (setq continuePass false))
        ;; Output the best record from all the block lists.
        (if continuePass
            (begin
               (setq outBlock[(length outBlock)] records[bestIndex])
               (setq records[bestIndex] (__readBlockList blockLists[bestIndex]))
               )) ;; end output if
        ;; Write the output block (if it is not empty).
        ;; Note: The output key is taken off the front
        ;;       of the list of output block keys. This
        ;;       means that the merged blocks are written
        ;;       back to the database using original keys.
        (if (>= (length outBlock) blockSize)
            (begin
               (setq outKey (append newBlockRoot "_" (setq outNum blockListOut[0])))
               (setq outExtNum (modi outNum myExtCount))
               (delete blockListOut 0) 
               (setq myRepos[outExtNum][outKey] (setAttributes outBlock #void))
               (setq outBlock (new Vector: object: 0))
               )) ;; end if
        ) ;; end while
    ;; Write the output block (if it is not empty).
    ;; Note: The output key is taken off the front
    ;;       of the list of output block keys. This
    ;;       means that the merged blocks are written
    ;;       back to the database using original keys.
    (if (> (length outBlock) 0)
        (begin
           (setq outKey (append newBlockRoot "_" (setq outNum blockListOut[0])))
           (setq outExtNum (modi outNum myExtCount))
           (delete blockListOut 0) 
           (setq myRepos[outExtNum][outKey] (setAttributes outBlock #void))
           (setq outBlock #void)
           )) ;; end if
    ;; Always return true.
    true) ;; end of __mergeMBlockLists























;;**EXPORTKEY**:dataMineLib:dmTableCursor:__privateMethods
(defChildLambda dataMineLib.dmTableCursor __privateMethods()
;; *******************************************************************
;; Summary:  Data Mine table cursor private child Lambdas.
;; Depends:  dataMineLib
;;           dmTableCursor
;; args:     errMsg	    The error message for this cursor.
;; Return:   error          This newly created table cursor error.
;; *******************************************************************
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> __clear #void) (goto Continue:))
    ;; Initialize private child Lambda methods.
    (defun __clear()
    ;; *******************************************************************
    ;; Summary: Clears the update global variables.
    ;; *******************************************************************
       (setq UPDInProgress false)
       (setq UPDIamDirty false)
       (setq UPDblockNUM #void)
       (setq UPDblockIDX #void)
       (setq UPDblockEXT #void)
       (setq UPDblockKEY #void)
       (setq UPDcurrentBlock #void)
       true) ;; end of __clear
    (defun __closeTableCursor()
       (if (<> myTableCursor #void)
           (setq myTableCursor (myDataMine.updateEnd myTableCursor)))
       true) ;; end of __closeTableCursor
    ;; *******************************************************************
    ;; summary:  Closes the table cursor (views only).
    ;; *******************************************************************
    (defun __cnvBlockExt(recnum) (modi (__cnvBlockNum recnum) myExtCount))
    ;; *******************************************************************
    ;; summary:  Returns the block extent from the record number
    ;; *******************************************************************
    (defun __cnvBlockIdx(recnum)
    ;; *******************************************************************
    ;; summary:  Returns the block record index from the record number
    ;; Args:     recnum           The record number to be converted.
    ;; Return:   The block record index for the record number. 
    ;; *******************************************************************
       (if (or (< recnum 0) (> recnum recordCount))
           (error "__cnvBlockIdx" "Attempt to convert illegal record number."))
       (modi recnum blockSize)) ;; end of __cnvBlockIdx
    (defun __cnvBlockInfo(recnum)
    ;; *******************************************************************
    ;; summary:  Sets the new block globals from the record number
    ;; Args:     recnum           The record number to be converted.
    ;; Return:   true
    ;; *******************************************************************
       (if (or (< recnum 0) (> recnum recordCount))
           (error "_cnvBlockInfo" "Attempt to convert illegal record number: " recnum))
       (setq newBlockIDX (modi recnum blockSize))
       (setq newBlockNUM (divi recnum blockSize))
       (setq newBlockEXT (modi newBlockNUM myExtCount))
       (setq newBlockKEY (append newBlockRoot "_" newBlockNUM)) 
       true) ;; end of __cnvBlockInfo
    (defun __cnvBlockKey(recnum) (append newBlockRoot "_" (__cnvBlockNum recnum)))
    ;; *******************************************************************
    ;; Summary:  Returns the block retrieval key from the record number
    ;; *******************************************************************
    (defun __cnvBlockNum(recnum)
    ;; *******************************************************************
    ;; summary:  Returns the block number from the record number
    ;; Args:     recnum           The record number to be converted.
    ;; Return:   The block number for the record number. 
    ;; *******************************************************************
       (if (or (< recnum 0) (> recnum recordCount))
           (error "__cnvBlockNum" "Attempt to convert illegal record number."))
       (divi recnum blockSize)) ;; end of __cnvBlockNum
    (defun __errorStop(errMsg)
    ;; *******************************************************************
    ;; Summary: Handles error conditions during sensitive operations.
    ;; *******************************************************************
       (if (isNumber fileID) (fileClose fileID 1))
       (setq fileID #void)
       (error (mid errMsg 1 (subi (length errMsg) 2)))) ;; end __errorStop
    (defun __makeBlockList(runStart runLength runEnd)
    ;; *******************************************************************
    ;; summary:  Return a vector containing the run sequence starting
    ;;           with the specified runStart and running for the
    ;;           specified runLength, but not to exceed the specified
    ;;           runEnd.
    ;; Args:     runStart   The starting integer of the run.
    ;;           runCount   The number of integers in the run.
    ;;           runEnd     The maximum integer in the run.
    ;; Return:   runVector  The run vector or #void.        
    ;; *******************************************************************
        vars:(runIndex             ;; Index of run integers
              runVector            ;; Result run vector
              ) ;; end temporary variables
        ;; Compute the number of integers in this run.
        ;; Note: We cannot exceed the specified runEnd.
        (if (>= (+ runStart runLength) runEnd)
            (setq runLength (- runEnd runStart)))
        (if (<= runLength 0)
            (return #void))
        ;; Create the run vector and fill it with sequential integers.
        (setq runVector (makeVector runLength))
        (setq runStart (integer runStart))
        (loop for runIndex from 0 until runLength do
            (setq runVector[runIndex] runStart)
            (setq runStart (addi runStart 1))
            ) ;; end fill loop
        runVector) ;; end of __makeBlockList
    (defun __readBlock(theRoot blockNum eraseSW)
    ;; *******************************************************************
    ;; summary:  Read (possibly destructively) the specified block.
    ;; *******************************************************************
        vars:(blockKey blockExtNum theBlock) 
        (setq blockKey (append theRoot "_" blockNum))
        (setq blockExtNum (modi blockNum myExtCount))
        (setq theBlock myRepos[blockExtNum][blockKey])
        (if eraseSW (setq myRepos[blockExtNum][blockKey] #void))
        (setAttributes theBlock colVector)
        theBlock) ;; end of __readBlock
    (defun __readBlockList(blockList)
    ;; *******************************************************************
    ;; summary:  Read the next record from the specified list of blocks.
    ;;           The blockList is a vector of block numbers. As a new block
    ;;           is read, it is stored in the cdr of the blockList. As
    ;;           a block is exhausted, it is erased from the repository,
    ;;           and the blockList is altered to reflect the erased block.
    ;; Args:     blockList   A vector of block keys to be read.
    ;; Return:   theRecord   The next table record (or #void if EOB).
    ;; *******************************************************************
        vars:(blockNum blockKey blockExtNum theBlock theRecord)
        ;; Return #void if there are no more records.
        (if (= blockList #void) (return #void))
        (if (and (= (cdr blockList) #void) (<= (length blockList) 0)) (return #void))
        ;; Make sure the current block is loaded from the repository.
        ;; Note: If we must load a new block, then we
        ;;       erase the block from the repository
        ;;       and from the blockList after we load.
        (if (= (cdr blockList) #void)
            (begin
               (setq blockKey (append tmpBlockRoot "_" (setq blockNum blockList[0])))
               (setq blockExtNum (modi blockNum myExtCount))
               (setCdr blockList (setAttributes myRepos[blockExtNum][blockKey] colVector))
               (setq myRepos[blockExtNum][blockKey] #void)
               (delete blockList 0)
               )) ;; end if
        ;; Read the next record from the current block.
        ;; Note: If we exhaust the current block, then we
        ;;       set the cdr of the blockList to #void.
        (setq theBlock (cdr blockList))
        (if (or (= theBlock #void) (<= (length theBlock) 0)) (return #void))
        (setq theRecord theBlock[0])
        (delete theBlock 0)
        (if (<= (length theBlock) 0) (setCdr blockList #void))
        ;; Return the next record.
        theRecord) ;; end of __readBlockList
    (defun __renameTmpBlocks()
    ;; *******************************************************************
    ;; Summary: Renames the current blocks as temporary blocks.
    ;; *******************************************************************
       (if (<> tmpBlockRoot #void) (__dropTemporary blockCount))
       (setq tmpBlockRoot newBlockRoot)
       (setq newBlockRoot (myDataMine.__newAutoName))
       (setq myIndex[myObjectName] (objectToStructure mySchema myParent.Pv))
       true) ;; end __renameTmpBlocks
    (defun __updateEnd()
    ;; *******************************************************************
    ;; Summary:  Make sure any dirty blocks are written to the table.
    ;; *******************************************************************
       ;; Save current block and update the table schema record?
       (if UPDIamDirty
          (begin
             (setq lastOperation #void)
             (setq myRepos[UPDblockEXT][UPDblockKEY] (setAttributes UPDcurrentBlock #void))
             (setq myIndex[myObjectName] (objectToStructure mySchema myParent.Pv))
          )) ;; end if
       (setq UPDIamDirty false)
       (setq UPDblockNUM #void)
       (setq UPDblockIDX #void)
       (setq UPDblockEXT #void)
       (setq UPDblockKEY #void)
       (setq UPDcurrentBlock #void)
       true) ;; end of __updateEnd
    (defun __updateRead(row)
    ;; *******************************************************************
    ;; Summary:  Reads a record from the table for later update.
    ;; Note:     Internal use only!!
    ;;           Reads do not have to be sequential.
    ;;           Writes do not have to be sequential.
    ;;           Each update session must be started with an updateBegin
    ;; Args:     row      Row number of row to be read for later update.
    ;; Return:   record   The row just read. 
    ;; *******************************************************************
       (if (or (< row 0) (> row recordCount)) (error "badRowIndex"))
       ;; Compute new block and record numbers.
       (__cnvBlockInfo row)
       ;; Need different block of records?
       (if (<> newBlockNUM UPDblockNUM)
           (begin
              ;; Save current block and update the table schema record?
              (if UPDIamDirty
                 (begin
                    (setq myRepos[UPDblockEXT][UPDblockKEY] (setAttributes UPDcurrentBlock #void))
                    (setq myIndex[myObjectName] (objectToStructure mySchema myParent.Pv))
                    )) ;; end if
              (setq UPDIamDirty false)
              (setq UPDblockKEY newBlockKEY)
              (setq UPDblockEXT newBlockEXT)
              (setq UPDblockNUM newBlockNUM)
              (setq UPDcurrentBlock myRepos[UPDblockEXT][UPDblockKEY])
              (if (= UPDcurrentBlock #void) (setq UPDcurrentBlock (new Vector: object: 0)))
              (setAttributes UPDcurrentBlock colVector)
              )) ;; end if
       ;; Return the requested record for later update.
       (setq UPDblockIDX newBlockIDX)
       UPDcurrentBlock[UPDblockIDX]) ;; end of __updateRead
    (defun __updateWrite(row record)
    ;; *******************************************************************
    ;; Summary:  Write a record to the table for update.
    ;; Note:     Internal use only!!
    ;;           Reads do not have to be sequential.
    ;;           Writes do not have to be sequential.
    ;;           Each update session must be started with an updateBegin
    ;; Args:     row      Row number of row to be read for later update.
    ;;           record   Row to be written to the table database.
    ;; Return:   record   The row just written. 
    ;; *******************************************************************
       (__updateRead row)
       (setq UPDIamDirty true)
       (setq lastOperation "updateWrite")
       ;; Update the record to the current block.
       (setAttributes record colVector)
       (setq UPDcurrentBlock[UPDblockIDX] record)
       (if (>= row recordCount) (setq recordCount (add1 row)))
       (if (>= UPDblockNUM blockCount) (setq blockCount (add1 UPDblockNUM)))
       ;; Return the record just written.
       record) ;; end of __updateWrite
    (defun __writeBlock(theRoot blockNum theBlock)
    ;; *******************************************************************
    ;; summary:  Write the specified block.
    ;; *******************************************************************
        vars:(blockKey blockExtNum) 
        (setq blockKey (append theRoot "_" blockNum))
        (setq blockExtNum (modi blockNum myExtCount))
        (setq myRepos[blockExtNum][blockKey] (setAttributes theBlock #void))
        (setAttributes theBlock colVector)
        theBlock) ;; end of __writeBlock
    ;; Initialize this data mine table cursor.
    Continue::
    true) ;; end dmTableCursor:__private methods






















;;**EXPORTKEY**:dataMineLib:dmTableCursor:__sortByRow
(defChildLambda dataMineLib.dmTableCursor __sortByRow()
;; *******************************************************************
;; summary:  Sort the entire view by table row number.
;; Notes:    This algorithm will increase the sort cluster factor
;;           during the critical initial sort phase (to match the
;;           available RAM memory). 
;; Args:     none
;; Return:   true        
;; *******************************************************************
    pvars:(;; Private Variable List
           (__testSW false)    ;; If test diagnostics are to be displayed, true; otherwise, false.
           __logFile           ;; Log file for testing purposes.
           ;; Private Methods List
           __sortReadBigBlock  ;; Read many smaller blocks into the UPDcurrentBlock, which becomes a giant block for sorting.
           __sortWriteBigBlock ;; Break the UPDcurrentBlock up into blocks of the correct blocking factor, then write each smaller block to the database.
           __testLog           ;; Read each record in the table and write contents to the test log.
           __testField         ;; Test field in each record to be written to the test log.
           ) ;; end of persistent variables
    vars:(blockLists           ;; Vector of block lists for each merge pass
          clusterIndex         ;; Index for scanning clustered blocks
          clusterSize          ;; Size of clustered blocks (always a log of 2)
          endTime              ;; Ending time for sort
          freeSpace            ;; Number of bytes of free space
          listCount            ;; Number of block list to create for each merge pass
          listIndex            ;; Index of the current block list to create
          mergeIndex           ;; Index of the current block to merge
          mergePassIndex       ;; Index of the current merge pass
          mergePassCount       ;; Number of merge passes required for this sort
          mergePassStart       ;; Starting value of the merge pass index
          mergeStepSize        ;; Number of blocks in each merge pass block list pair
          mergeRunCount        ;; Number of blocks in each merge pass block list
          recordOne            ;; Record one for merge
          recordTwo            ;; Record two for merge
          startTime            ;; Starting time for sort
          ) ;; end temporary variables
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> __sortReadBigBlock #void) (goto Continue:))
    (defun sortLambda(x y) (< x.__row y.__row))
    (defun __sortReadBigBlock(blockIndex clusterSize blockCount)
        ;; Read many smaller blocks into the UPDcurrentBlock,
        ;; which becomes a giant block for sorting.
        ;; Note: We have already read the first block 
        ;;       into UPDcurrentBlock.
        vars:(newBlock clusterIndex stopIndex)
        (setq stopIndex (min clusterSize (subi blockCount blockIndex)))
        (loop for clusterIndex from 1 until stopIndex do
            (setq newBlock (__readBlock newBlockRoot (addi blockIndex clusterIndex) false))
            (if __testSW 
                (begin 
                   (gc) 
                   (fwriteln __logFile "Read block [" (addi blockIndex clusterIndex) "] size = [" (sizeof newBlock) "] free = [" (inspect))  
                )) ;; end diagnostic if
            (setq UPDcurrentBlock (append UPDcurrentBlock newBlock))
            ) ;; end loop
        true) ;; end of __sortReadBigBlock
    (defun __sortWriteBigBlock(blockIndex)
        ;; Break the UPDcurrentBlock up into blocks of the correct blocking
        ;; factor, then write each smaller block to the database.
        vars:(newBlock clusterIndex clusterSize recordIndex blockStart blockLen)
        (setq clusterSize (ceiling (/ (length UPDcurrentBlock) blockSize)))
        (if (= clusterSize 1) (return (__writeBlock newBlockRoot blockIndex UPDcurrentBlock)))
        (loop for clusterIndex from 0 until clusterSize do
            (setq blockStart (muli clusterIndex blockSize))
            (setq newBlock (new Vector: object: (min blockSize (subi (length UPDcurrentBlock) blockStart))))
            (setq blockLen (length newBlock))
            (loop for recordIndex from 0 until blockLen do
                (setq newBlock[recordIndex] UPDcurrentBlock[(addi recordIndex blockStart)])
                ) ;; end recordIndex loop
            (__writeBlock newBlockRoot (addi blockIndex clusterIndex) newBlock)
            ) ;; end clusterIndex loop
        true) ;; end of __sortWriteBigBlock
    (defun __testLog()
        ;; Read each record in the table and write contents to the test log.
        vars:(recordIndex theRecord)
        (loop for recordIndex from 0 until recordCount do
            (setq theRecord (__updateRead recordIndex))
            (fwriteln __logFile "rec[" recordIndex "]." __testField " = " theRecord[__testField])  
            ) ;; end loop
        (__updateEnd)) ;; end of __testLog
    ;; Initialize the Lambda when called by new ObjectRepository: or attachLibrarian. 
    ;; This function always reattaches itself to the Repository (if necessary),
    ;; and always reestablishes the table block count, record count, and other statistics.
    Continue::
    ;; Sort the records in each block.
    ;; Note: This is the first part of a merge  
    ;;       sort of the entire table database.
    (if (<> myObjectType view:) (error "__sortByRow: Attempt to sort a non view by table row: " myObjectName))
    (if __testSW (setq __logFile (fileOpen "sortlog.txt" 1 0)))
    (setq startTime (getTickCount 0))
    (gc)
    (setq freeSpace (- (inspect) 10000000))
    (if (isNegative freeSpace) (error "__sortByRow: Insufficient workspace memory to perform a data mine table sort."))
    (setq clusterSize 1)
    (loop for UPDblockNUM from 0 until blockCount do
        (setq UPDcurrentBlock (__readBlock newBlockRoot UPDblockNUM false))
        ;; Compute the cluster size (if this is the first block)
        (if (and (= UPDblockNUM 0) (isPositive freeSpace))
            (begin
               (setq clusterSize (divi freeSpace (sizeof UPDcurrentBlock) 4))
               (setq clusterSize (expt 2 (integer (log2 clusterSize))))
               ;(writeln "clusterSize = [" clusterSize "]")
               (if __testSW (fwriteln __logFile "Cluster size = [" clusterSize "]"))
               )) ;; end if
        ;; Read and sort blocks in clusters according to cluster size
        (__sortReadBigBlock UPDblockNUM clusterSize blockCount)
        (^sort UPDcurrentBlock (lambda(x y) (< x.__row y.__row)))
        (__sortWriteBigBlock UPDblockNUM)
        ;; Adjust block index for size of cluster
        (setq UPDblockNUM (sub1 (addi UPDblockNUM clusterSize)))
        ) ;; end of loop
    (setq UPDcurrentBlock #void)
    (setq UPDblockNUM #void)
    (setq UPDblockIDX #void)
    (setq UPDblockEXT #void)
    (setq UPDblockKEY #void)
    (if __testSW 
        (begin
           (fwriteln __logFile "Completed first pass sort.")
           (__testLog)
           )) ;; end test if
    ;; Perform the merge phase of the sort.
    ;; Note:  The number of merge passes is always equal
    ;;        to the log 2 of the number of blocks.
    ;; For Example:
    ;;        The merge phase of the sort for a database
    ;;        of seven blocks would appear as follows.
    ;;        (__merge2BlockLists (makeVector 1 0) (makeVector 1 1) sortLambda)
    ;;        (__merge2BlockLists (makeVector 1 2) (makeVector 1 3) sortLambda)
    ;;        (__merge2BlockLists (makeVector 1 4) (makeVector 1 5) sortLambda)
    ;;        (__merge2BlockLists (makeVector 1 6) (makeVector 0 0) sortLambda)
    ;;        (__merge2BlockLists (makeVector 2 0 1) (makeVector 2 2 3) sortLambda)
    ;;        (__merge2BlockLists (makeVector 2 4 5) (makeVector 1 6) sortLambda)
    ;;        (__merge2BlockLists (makeVector 4 0 1 2 3) (makeVector 3 4 5 6) sortLambda)
    (if (< clusterSize blockCount)
        (begin
           (setq mergePassCount (integer (log2 blockCount)))
           (setq mergePassStart (integer (log2 clusterSize)))
           (setq listCount (expt 2 mergePassStart))
           (if (<> mergePassCount (log2 blockCount)) (++ mergePassCount))
           (setq blockLists (new Vector: listCount))
           (setq mergePassIndex mergePassStart)
           (while (< mergePassIndex mergePassCount) do
               (if __testSW (fwriteln __logFile "Starting merge pass [" mergePassIndex "] of [" mergePassCount "]"))
               (setq mergeStepSize (expt 2 (add1 mergePassIndex)))
               (setq mergeRunCount (divi mergeStepSize 2))
               (setq mergeIndex 0)
               (__renameTmpBlocks)
               (while (< mergeIndex blockCount)
                   (setq listIndex 0)
                   (while (< listIndex listCount) do
                      (setq blockLists[listIndex] (__makeBlockList mergeIndex mergeRunCount blockCount))
                      (+= mergeIndex mergeRunCount)
                      (++ listIndex)
                      ) ;; end block list creation loop
                   ;; Write to test log (if necessary)..
                   (__mergeMBlockLists sortLambda blockLists)
                   ) ;; end merge stepping while
               (if __testSW 
                   (begin
                      (fwriteln __logFile "Completed merge pass [" mergePassIndex "] of [" mergePassCount "]")
                      (__testLog)
                      )) ;; end test if
               (+= mergePassIndex mergePassStart)
               ) ;; end merge passes loop
           )) ;; end merge if
    (setq endTime (integer (getTickCount startTime)))
    (setq rowOrderSW true)
    (if __testSW
        (begin
           (fwriteln __logFile "Records sorted = " recordCount " in " endTime " Seconds" )
           (setq __logFile (fileClose __logFile 1))
           )) ;; end test if
    true) ;; end of __sortByRow






















;;**EXPORTKEY**:dataMineLib:dmTableCursor:__truncateEnds
(defChildLambda dataMineLib.dmTableCursor __truncateEnds(begNumber endNumber)
;; *******************************************************************
;; summary:  Truncate records from both ends of a table..
;; Args:     begNumber     Truncate these records from the start of the table.
;;           endNumber     Truncate these records from the end of the table.
;; Return:   recordCount   The number of center records selected.     
;; *******************************************************************
    vars:(cursor begRecordNUM midRecordNUM endRecordNUM theRecord)
    ;; Are we truncating nothing?
    (if (and (= begNumber 0) (= endNumber 0)) (return recordCount))
    ;; Are we truncating everything?
    (if (>= (addi begNumber endNumber) recordCount) (return (__dropAllRecords)))
    (__updateEnd)
    ;; Are we deleting no records from the beginning?
    (if (<= begNumber 0)
        (return (__truncateMid (subi recordCount endNumber) 0))
        ) ;; end if
    ;; We are dropping some records from the beginning.
    ;; Determine the record number of the first end record to be kept,
    ;; and the record number of the first middle record to be kept,
    (setq endRecordNUM (subi recordCount endNumber))
    (setq midRecordNUM (addi begNumber 1))
    (setq begRecordNUM 0)
    ;; Seamlessly join the remaining end records with the beginning records.
    (setq cursor (myDataMine.updateBegin myObjectName))
    (while (< midRecordNUM endRecordNUM) do 
        (setq theRecord (cursor.__updateRead midRecordNUM))
        (__updateWrite begRecordNUM theRecord)
        (++ midRecordNUM)
        (++ begRecordNUM)
        ) ; end while
    (__updateEnd)
    (myDataMine.updateEnd cursor)
    (setq cursor #void)
    ;; Now truncate the table on the end to delete the unwanted records.
    (__truncateMid (subi recordCount begNumber endNumber) 0)) ;; end of __truncateEnds





















;;**EXPORTKEY**:dataMineLib:dmTableCursor:__truncateMid
(defChildLambda dataMineLib.dmTableCursor __truncateMid(begNumber endNumber)
;; *******************************************************************
;; summary:  Truncate records from the center of a table.
;; Args:     begNumber     Keep these records from the start of the table.
;;           endNumber     Keep these records from the end of the table.
;; Return:   recordCount   The number of records selected.     
;; *******************************************************************
    vars:(cursor endRecordNUM begRecordNUM theRecord)
    ;; Are we already truncated?
    (if (>= (addi begNumber endNumber) recordCount) (return recordCount))
    (__updateEnd)
    ;; Are we keeping no records from the end?
    (if (<= endNumber 0)
        (begin
           ;; Compute new block and record numbers.
           (setq UPDblockNUM (__cnvBlockNum begNumber))
           (setq UPDblockIDX (__cnvBlockIdx begNumber))
           (setq UPDblockEXT (__cnvBlockExt begNumber))
           (setq UPDblockKEY (__cnvBlockKey begNumber))
           (setq UPDcurrentBlock  (__readBlock newBlockRoot UPDblockNUM false))
           (setq UPDIamDirty true)
           ;; Delete all trailing blocks from the table.
           (__deleteBlocks newBlockRoot (add1 UPDblockNUM) blockCount)
           (setq blockCount (addi UPDblockNUM 1))
           (setq recordCount begNumber)
           ;; Resize the last block to reflect the truncated records.
           (resize UPDcurrentBlock (add1 UPDblockIDX))
           (__updateEnd)
           (return recordCount)
           )) ;; end if
    ;; We are keeping some records from the beginning.
    ;; Determine the record number of the first end record to be kept,
    ;; and the record number of the first middle record to be deleted,
    (setq endRecordNUM (subi recordCount endNumber))
    (setq begRecordNUM begNumber)
    ;; Seamlessly join the remaining end records with the beginning records.
    (setq cursor (myDataMine.updateBegin myObjectName))
    (while (< endRecordNUM recordCount) do 
        (setq theRecord (cursor.__updateRead endRecordNUM))
        (__updateWrite begRecordNUM theRecord)
        (++ endRecordNUM)
        (++ begRecordNUM)
        ) ; end while
    (__updateEnd)
    (myDataMine.updateEnd cursor)
    (setq cursor #void)
    ;; Now truncate the table on the end to delete the unwanted records.
    (__truncateMid (+ begNumber endNumber) 0)) ;; end of __truncateMid










;;**EXPORTKEY**:dataMineLib:dmTableCursor:backupMethods
(defChildLambda dataMineLib.dmTableCursor backupMethods()
;; *******************************************************************
;; Summary:  Data Mine table cursor private child Lambdas.
;; Depends:  dataMineLib
;;           dmTableCursor
;; args:     errMsg	    The error message for this cursor.
;; Return:   error          This newly created table cursor error.
;; *******************************************************************
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> backup #void) (goto Continue:))
    ;; Initialize private child Lambda methods.
    (defun backup()
    ;; *******************************************************************
    ;; Summary: Creates a backup copy of the data mine table.
    ;; *******************************************************************
       (__updateEnd)
       (if (<> bckBlockRoot #void)
           (__deleteBlocks bckBlockRoot 0 bckSchema.blockCount)
           (setq bckBlockRoot (myDataMine.__newAutoName))
           ) ;; end if
       (setq bckSchema (objectToStructure (copy mySchema) myParent.Pv))
       (__copyBlocks bckBlockRoot newBlockRoot 0 blockCount)
       (setq myIndex[myObjectName] (objectToStructure mySchema myParent.Pv))
       true) ;; end of backup
    (defun checkPoint()
    ;; *******************************************************************
    ;; Summary: Creates a check point copy of the data mine table.
    ;; *******************************************************************
       (__updateEnd)
       (if (<> ckpBlockRoot #void)
           (__deleteBlocks ckpBlockRoot 0 ckpSchema.blockCount)
           (setq ckpBlockRoot (myDataMine.__newAutoName))
           ) ;; end if
       (setq ckpSchema (objectToStructure (copy mySchema) myParent.Pv))
       (__copyBlocks ckpBlockRoot newBlockRoot 0 blockCount)
       (setq myIndex[myObjectName] (objectToStructure mySchema myParent.Pv))
       true) ;; end of checkPoint
    (defun restore()
    ;; *******************************************************************
    ;; Summary: Restores the backup copy of the data mine table.
    ;; *******************************************************************
       (__updateEnd)
       (if (= bckBlockRoot #void) (error "__restore" "There is no backup copy to restore."))
       (__deleteBlocks newBlockRoot 0 blockCount)
       (setq blockCount bckSchema.blockCount)
       (setq blockSize bckSchema.blockSize)
       (setq colCount bckSchema.colCount)
       (setq colVector bckSchema.colVector)
       (setq recordCount bckSchema.recordCount)
       (setq recordStructure bckSchema.recordStructure)
       (setq rowOrderSW bckSchema.rowOrderSW)
       (setq validFields bckSchema.validFields)
       (__copyBlocks newBlockRoot bckBlockRoot 0 blockCount)
       (setq myIndex[myObjectName] (objectToStructure mySchema myParent.Pv))
       true) ;; end of restore
    (defun rollBack()
    ;; *******************************************************************
    ;; Summary: Restores the check point copy of the data mine table.
    ;; *******************************************************************
       (__updateEnd)
       (if (= ckpBlockRoot #void) (error "__rollBack" "There is no check point copy to roll back."))
       (__deleteBlocks newBlockRoot 0 blockCount)
       (setq blockCount ckpSchema.blockCount)
       (setq blockSize ckpSchema.blockSize)
       (setq colCount ckpSchema.colCount)
       (setq colVector ckpSchema.colVector)
       (setq recordCount ckpSchema.recordCount)
       (setq recordStructure ckpSchema.recordStructure)
       (setq rowOrderSW bckSchema.rowOrderSW)
       (setq validFields ckpSchema.validFields)
       (__copyBlocks newBlockRoot ckpBlockRoot 0 blockCount)
       (setq myIndex[myObjectName] (objectToStructure mySchema myParent.Pv))
       true) ;; end of rolBack
    ;; Initialize this data mine table cursor.
    Continue::
    true) ;; end backupMethods
















;;**EXPORTKEY**:dataMineLib:dmTableCursor:compileFilter
(defChildLambda dataMineLib.dmTableCursor compileFilter(aSpec)
;; *******************************************************************
;;  Summary: Builds a data mine table filter Lambda from the ACL specification   
;;           passed as an argument. The ACL specification is designed
;;           to allow ease of specification.
;; Depends:  rulesLib
;; Args:     aSpec      Fixup specification language text.
;; Return:   proc       A table fixup Lambda.   
;; *******************************************************************
   pvars:((debugOn false)         ;; Display generated Lambda on switch
          filterFields            ;; Temporary Dictionary of valid field names
          fixupLambda              ;; Lambda for expression transformation
          syntaxLambda             ;; Lambda for expression syntax validation
          ;; Methods list 
          assertRules             ;; Method for defining fix up rules
          buildBody               ;; Method to build the SmartLisp expression body
          buildName               ;; Method for recognizing a Legacy field name
          cmdAll                  ;; Method for parsing the "all" command
          cmdOmit                 ;; Method for parsing the "omit" command
          cmdSort                 ;; Method for parsing the "bottom" and "top" commands
          errorStop               ;; Method for error recovery
          mergeRules              ;; Method for merging "all" criteria together
          pairToVector            ;; Method for converting a list into a vector
          replaceRules            ;; Method for recursive criteria replacement
          ) ;; end of persistent variables
   vars:(proc aCmd i j m n cmds scmd theSpec cmdVector)
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> cmdAll #void) (goto Continue:))
   ;; Initialize the inline child Lambdas.
   (defun assertRules()
       ;; We only need to run this once.
       (if (isLambda fixupLambda) (return true))
       ;; Define the expression fix up rules.
       (rulesLib)
       (setq fixupLambda (new rulesLib))
       ;; Make sure there are no duplication errors
       (if (= fixupLambda.rulesDic rulesLib.rulesDic) (error "dupRules"))
       ;; Operator name, function name, and number recognition rules
       (fixupLambda.assert $OP:(lambda(x) 
                                 vars:(s (d #{and and or or min min max max + + - - * * / / ** expt < < <= <= = = <> <> >= >= > >})) 
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $REL:(lambda(x) vars:((d #{< < <= <= = = <> <> >= >= > >})) (if (isMember x d) d[x])))
       (fixupLambda.assert $FN1:(lambda(x) 
                                 vars:(s (d #{not not sin sin cos cos tan tan log log exp exp sqrt sqrt})) 
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $FN2:(lambda(x) 
                                 vars:(s (d #{min min max max}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $NAM:(lambda(x) 
                                 vars:((d #(today))) 
                                 (if (not (isMember x d)) x))
                                 ) ;; end assert
       (fixupLambda.assert $NUM:(lambda(x) (if (isNumber x) x)))
       ;; Infix to Prefix notation production rules
       (fixupLambda.assert $PFN:(lambda(a fn x y b) 
                                   vars:(p)
                                   (setq p (list fn x y)) 
                                   (if (<> b #void) (setq p (append (list p) b))) 
                                   (if (<> a #void) (setq p (append a p))) 
                                   p))
       (fixupLambda.assert '($a* $X <$FN=$OP> $Y $b*) '(<$PFN> $a $FN $X $Y $b))
       ;; Constant folding production rules
       (fixupLambda.assert $DIV:(lambda(x) (error "compileFilter" (append "compileFilter divide by zero on: " (string x true)))))
       (fixupLambda.assert $FOLD1:(lambda(op x) vars:(f) (setq f (getGlobalValue (symbol op))) (f x)))
       (fixupLambda.assert $FOLD2:(lambda(op x y) vars:(f) (setq f (getGlobalValue (symbol op))) (f x y)))
       (fixupLambda.assert '(<$Y=$OP> <$X=$NUM> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '(<$Y=$FN1> <$X=$NUM>) '(<$FOLD1> $Y $X))
       (fixupLambda.assert '(<$Y=$FN2> <$X=$NUM> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '(** $X $Y) '(expt $X $Y))
       ;; Algebraic expression reduction rules
       (fixupLambda.assert '(/ $X $X) 1)
       (fixupLambda.assert '(+ $X 0) '$X)
       (fixupLambda.assert '(- $X 0) '$X)
       (fixupLambda.assert '(* $X 0) 0)
       (fixupLambda.assert '(/ 0 0) 0)
       (fixupLambda.assert '(/ $X 0) '(<$DIV> (/ $X 0)))
       (fixupLambda.assert '(expt $X 0) 1)
       (fixupLambda.assert '(+ 0 $X) '$X)
       (fixupLambda.assert '(* 0 $X) 0)
       (fixupLambda.assert '(/ 0 $X) 0)
       (fixupLambda.assert '(expt 0 $X) 0)
       (fixupLambda.assert '(* $X 1) '$X)
       (fixupLambda.assert '(/ $X 1) '$X)
       (fixupLambda.assert '(expt $X 1) '$X)
       (fixupLambda.assert '(* 1 $X) '$X)
       (fixupLambda.assert '(expt 1 $X) 1)
       ;; _ANY_ empty indices reduction rules
       (fixupLambda.assert '(<$O=$REL> (ref (ref (ref x $X)) $Y) $Z) 
                          '(_ANY_ ($O (ref x $Y) $Z) (ref x $X)))
       ;; One based indices reduction rules
       (fixupLambda.assert '(ref (ref x $X) <$Y=$NUM>) '(ref (ref x $X) (sub1 $Y)))
       ;; Excess parentheses reduction rules
       (fixupLambda.assert '(($X*)) '$X)
       (fixupLambda.assert '(<$X=$NAM>) '$X)
       ;; Define the expression syntax validation rules.
       (setq syntaxLambda (new rulesLib))
       (syntaxLambda.setFailure false)
       ;; Make sure there are no duplication errors
       (if (= syntaxLambda.rulesDic fixupLambda.rulesDic) (error "dupRules"))
       ;; Prefix notation recognition rules
       (syntaxLambda.assert '(ref $X $Z) false)
       (syntaxLambda.assert '(<$Y=$FN1> <$X=$NUM>) '(<$FOLD1> $Y $X))
       (syntaxLambda.assert '(<$Y=$OP> $X $Z) false)
       (syntaxLambda.assert '(<$Y=$FN1> $X) false)
       (syntaxLambda.assert '(<$Y=$FN2> $X $Z) false)
       (syntaxLambda.assert '(today) false)
       ;; _ANY_ macro substitution rules
       (syntaxLambda.assert '(_ANY_ $A $B) 
                           '(mapc (lambda(x) (let ((r r)) (if $A (setq r true)) r)) $B))
       ;; Unrecognized syntax error rules
       (syntaxLambda.assert $ERR:(lambda(x) (error "compileFilter" (append "compileFilter syntax error on: " (string x true)))))
       (syntaxLambda.assert '$ALL$ '(<$ERR> $ALL$))
       ;; Operator and function name recognition lambda rules
       (syntaxLambda.assert $OP:(lambda(x) 
                                 vars:(s (d #{and and or or min min max max + + - - * * / / expt expt < < <= <= = = <> <> >= >= > >}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FN1:(lambda(x) 
                                 vars:(s (d #{isBoolean isBoolean isNumber isNumber isDate isDate isSymbol isSymbol isString isString isDictionary isDictionary not not sin sin sub1 sub1 cos cos tan tan log log exp exp sqrt sqrt}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FN2:(lambda(x)
                                 vars:(s (d #{min min max max mapc mapc}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FOLD1:(lambda(op x) vars:(f) (setq f (getGlobalValue (symbol op))) (f x)))
       (syntaxLambda.assert $NUM:(lambda(x) (if (isNumber x) x)))
       true) ;; end assertRules
   (defun buildBody(v p begIndex endIndex)
   ;; Define the build expression body function. This function builds SmartLisp
   ;;  expression bodies.
   ;;  such as (1 {A1} true etc).
       vars:(fieldName s i n)
       (setq s "")
       (loop for i from begIndex until endIndex do
           (setq s (append s " " (buildName v[i] p)))
           ) ;; end loop
       ;; Apply expression transformation rules.
       (if (= p "x")
           (fixupLambda.setVerbose false)
           (fixupLambda.setVerbose false))
       (setq s (lisp s arithmetic:))
       (setq fixupLambda.singlePass false)
       (setq s (fixupLambda.apply s))
       (setq s (syntaxLambda.apply s))
       (setq s (string s true))
       s) ;; end buildBody
   (defun buildName(s p)
   ;; Define the build name function. This function recognizes Legacy
   ;;  field names inclosed in vertical bar | symbols, or SmarLisp constants
   ;;  such as (1 {A1} true etc).
       vars:(fieldName)
       (cond ;; Recognize a Legacy field name inclosed in vertical bar | symbols.
             ;;  Return the construct: p.|Fieldname|
             ((and (isType Symbol: s) (= s[0] #\|))
              (begin
                 (setq fieldName s)
                 (if (= (member fieldName validFields) false)
                     (error "buildName"))
                 (setq filterFields[fieldName] validFields[fieldName])
                 (append p "." fieldName)))
             ;; Recognize a Legacy field name not inclosed in vertical bar | symbols.
             ;;  Return the construct: p.|Fieldname|
             ((and (isType Symbol: s) (<> (member s validFields) false))
              (begin
                 (setq fieldName s)
                 (setq filterFields[fieldName] validFields[fieldName])
                 (append p "." fieldName)))
             ;; Recognize the today operator symbols.
             ;;  Convert the symbol to a List contant.
             ((= s today:)
              (append "(" s ")"))
             ;; Recognize all other non-arithmetic operator symbols.
             ;;  Convert the symbol to a string contant.
             ((and (isType Symbol: s) (= (member s #(isBoolean isNumber isDate isSymbol isString isDictionary and or not + - * / < > >= <= = <> ref |(| |)| |[| |]|)) false))
              (append "{" s "}"))
             ;; Repackage string constants within braces.
             ;;  Convert the string to a string contant.
             ((and (isString s) (= (member s #(isBoolean isNumber isDate isSymbol isString isDictionary and or not + - * / < > >= <= = <> ref |(| |)| |[| |]|)) false))
              (append "{" s "}"))
             ;; Recognize all quoted symbols.
             ;;  Convert the quoted symbol to a string contant.
             ((isType QuotedSymbol: s)
              (append "{" s "}"))
             ;; Assume we have a SmartLisp constant.
             ;;  Return the constant unaltered.
             (else s)
             ) ;; end cond
        ) ;; end buildName
   (defun cmdAll(parms s)
   ;; Define the "all" command parser.
   ;;   for example: "all = |Timeliness| 1"
       vars:(parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (= parmCnt 1)
           (return "")) 
       (if (< parmCnt 4)
           (error "compileFilter" s)) 
       (if (<> parms[0] "all")
           (error "compileFilter" s)) 
       (if (and (= parmCnt 4)
                (isBoolean (member parms[2] #("=" "<>" "<=" ">=" "<" ">"))))
           (error "compileFilter" s))
       ;; Build the resulting find command.
       (setq outS (append "(TBC.truncate (lambda(x) (onError (lambda(s) false)) "
                          (buildBody parms "x" 1 parmCnt) "))"))
       outS) ;; end cmdAll
   (defun cmdOmit(parms s)
   ;; Define the "all" command parser.
   ;;   for example: "omit |Timeliness| = 5"
       vars:(parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (= parmCnt 1)
           (return "")) 
       (if (< parmCnt 4)
           (error "compileFilter" s)) 
       (if (<> parms[0] "omit")
           (error "compileFilter" s)) 
       (if (and (= parmCnt 4)
                (isBoolean (member parms[2] #("=" "<>" "<=" ">=" "<" ">"))))
           (error "compileFilter" s))
       ;; Build the resulting find command.
       (setq outS (append "(TBC.truncate (lambda(x) (onError (lambda(s) false)) (not "
                          (buildBody parms "x" 1 parmCnt) ")))"))
       outS) ;; end cmdOmit
   (defun cmdSort(parms s)
   ;; Define the "top", "bottom", & "sort" command parsers.
   ;;   for example: "top |Price| 5"
       vars:(parmCnt lastParm outS n ratio sortOp)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       (setq lastParm (subi parmCnt 1))
       ;; Check command for number of arguments.
       (cond 
           ;; Manage simple truncate: "top cutoff"
           ((and (= parmCnt 2) (isMember parms[0] #(top: bottom:)))
            (begin 
               (if (= parms[0] "top")
                   (error "topInvalid")
                   (setq sortOp "<="))
               (cond ((isInteger parms[1])
                      (setq ratio (append "" parms[1])))
                     ((and (isNumber parms[1]) (< parms[1] 1))
                      (setq ratio (append "(integer (* TBC.recordCount " parms[1] "))")))
                     ((= parms[1] "all")
                      (setq ratio "TBC.recordCount"))
                     (else
                      (error "compileFilter" parms))
                   ) ;; end cond
               ;; Build the resulting find command.
               (setq outS (append "(TBC.truncate " ratio ")"))))
           ;; Manage simple field sort: "top,value1,cutoff"
           ((and (= parmCnt 3) (isMember parms[0] #(top: bottom:)))
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">=")
                   (setq sortOp "<="))
               (cond ((isInteger parms[2])
                      (setq ratio (append "" parms[2])))
                     ((and (isNumber parms[2]) (< parms[2] 1))
                      (setq ratio (append "(integer (* TBC.recordCount " parms[2] "))")))
                     ((= parms[2] "all")
                      (setq ratio "TBC.recordCount"))
                     (else
                      (error "compileFilter" parms))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(TBC.sort (lambda(x y) (onError (lambda(s) false)) (" sortOp
                                  " " (buildName parms[1] "x")
                                  " " (buildName parms[1] "y") "))" _eol "         "
                                  "(lambda(x) (onError (lambda(s) false))"
                                  " " (buildName parms[1] "x") ")" ;;  _eol "         "
                                  " |" sortOp "|:) ; end sort" _eol "   "))
               (setq outS (append outS "(TBC.truncate " ratio ")"))))
           ;; Manage simple expression sort: "top,value1,/,value2,cutoff" 
           ;; Note: (for backward compatibility).
           ((and (= parmCnt 5) 
                 (isMember parms[0] #(top: bottom:)) 
                 (isNumber (member parms[2] #("+" "-" "*" "/"))))
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">=")
                   (setq sortOp "<="))
               (cond ((isInteger parms[4])
                      (setq ratio (append "" parms[4])))
                     ((and (isNumber parms[4]) (< parms[4] 1))
                      (setq ratio (append "(integer (* TBC.recordCount " parms[4] "))")))
                     ((= parms[4] "all")
                      (setq ratio "TBC.recordCount"))
                     (else
                      (error "compileFilter" parms))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(TBC.sort (lambda(x y) (onError (lambda(s) false)) (" sortOp
                                   " (" parms[2] " " (buildName parms[1] "x") " " (buildName parms[3] "x") ")"
                                   " (" parms[2] " " (buildName parms[1] "y") " " (buildName parms[3] "y") ")"
                                   "))" _eol "         "
                                   "(lambda(x) (onError (lambda(s) false))"
                                   " (" parms[2] " " (buildName parms[1] "x") " " (buildName parms[3] "x") "))"
                                   " |" sortOp "|:) ; end sort" _eol "   "))
               (setq outS (append outS "(TBC.truncate " ratio ")"))))
           ;; Manage complex expression sort: "top,/,value1,value1,cutoff" 
           (else
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">=")
                   (setq sortOp "<="))
               (cond ((isInteger parms[lastParm])
                      (setq ratio (append "" parms[lastParm])))
                     ((and (isNumber parms[lastParm]) (< parms[lastParm] 1))
                      (setq ratio (append "(integer (* TBC.recordCount " parms[lastParm] "))")))
                     ((= parms[lastParm] "all")
                      (setq ratio "TBC.recordCount"))
                     (else
                      (error "compileFilter" parms))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(TBC.sort (lambda(x y) (onError (lambda(s) false)) (" sortOp " "
                                   (buildBody parms "x" 1 lastParm) " "
                                   (buildBody parms "y" 1 lastParm) "))"  _eol "         "
                                   "(lambda(x) (onError (lambda(s) false))"
                                   " " (buildBody parms "x" 1 lastParm) ")"
                                   " |" sortOp "|:) ; end sort" _eol "   "))
               (setq outS (append outS "(TBC.truncate " ratio ")"))))
           ) ;end cond
       outS) ;; end cmdSort
   (defun errorStop(err) (writeln "compileFilter: " err) false)
   (defun mergeRules(cmds)      
   ;; Merges consecutive "all" rules together with an "and" conjunction.
   ;; Looks for "all" rules and eliminates them.
       vars:(cmdIndex args newCmds ruleLambda i)
       (loop for cmdIndex from 0 until (length cmds) do
           (cond
              ;; Replace "all" command with nothing,
              ;; because and "all" command is a noop.
              ((= cmds[cmdIndex] "all")
               (begin
                  (setq cmds (delete cmds cmdIndex))
                  (setq cmdIndex (max (- cmdIndex 2) -1))
                  )) ;; end begin
              ;; Replace "remarks" command with nothing,
              ;; because and "remarks" command is a comment.
              ((= (left cmds[cmdIndex] 7) "remarks")
               (begin
                  (setq cmds (delete cmds cmdIndex))
                  (setq cmdIndex (max (- cmdIndex 2) -1))
                  )) ;; end begin
              ;; Replace "omit" command with "all (not )",
              ((= (left cmds[cmdIndex] 5) "omit ")
               (begin
                  (setq newCmds (append "all (not ("
                                        (mid cmds[cmdIndex] 5 100000)
                                        "))"))
                  (setq cmds[cmdIndex] newCmds)
                  (setq cmdIndex (max (- cmdIndex 2) -1))
                  )) ;; end begin
              ;; Merge consecutive "all" commands together,
              ;; with a conjunctive "and" operator.
              ((and (= (left cmds[cmdIndex] 4) "all ")
                    (< (add1 cmdIndex) (length cmds))
                    (= (left cmds[(add1 cmdIndex)] 4) "all "))
               (begin
                  (setq newCmds (append "all ("
                                        (mid cmds[cmdIndex] 4 100000)
                                        ") and ("
                                        (mid cmds[(add1 cmdIndex)] 4 100000)
                                        ")"))
                  (setq cmds (delete cmds cmdIndex))
                  (setq cmds[cmdIndex] newCmds)
                  (setq cmdIndex (max (sub1 cmdIndex) -1))
                  )) ;; end begin
               ) ;; end cond
           ) ;; end of loop
       cmds) ;; end of mergeRules
   (defun pairToVector(v p)
       vars:(i n result)
       (if (= v #void) (setq v (new Vector: 0)))
       (setq n (length p))
       (loop for i from 0 until n do
          (if (isAtom p[i])
              (setq v[(length v)] p[i])
              (begin
                 (setq v[(length v)] |(|:)
                 (setq v (pairToVector v p[i]))
                 (setq v[(length v)] |)|:)
                 )) ;; end if
          ) ;; end loop
       v) ;; end pairToVector
   (defun replaceRules(cmds)
   ;; Recursively replaces rules with their command strings.
   ;; Looks for "fixup" rules in the parent data mine Lambda.
       vars:(cmdIndex args newCmds i)
       (loop for cmdIndex from 0 until (length cmds) do
           ;; Check for rule command
           (if (= (left cmds[cmdIndex] 8) "fixup")
               (begin
                  (setq args (objectToVector (lisp cmds[cmdIndex] arithmetic:)))
                  ;; Retrieve the named fix up Lambda source
                  ;; and covert to a vector of commands
                  (if (and (= (left args[1] 1) "{") (= (right args[1] 1) "}")) 
                      (setq args[1] (mid args[1] 1 (subi (length args[1]) 2))))
                  (setq newCmds (myDataMine.__getFixupSource args[1]))
                  (setq newCmds (stringToVector newCmds ";"))
                  ;; Replace substitute rules within the original 
                  ;; vector of command strings.
                  (setq cmds[cmdIndex] newCmds[0])
                  (loop for i from 1 until (length newCmds) do
                      (setq cmds (vectorInsert cmds (addi cmdIndex i) newCmds[i]))
                      ) ;; end loop
                  ;; Make sure we retest the first substituted rule.
                  (-- cmdIndex)
                  )) ;; end if
           ) ;; end of loop
       cmds) ;; end of replaceRules
   ;; Initialize the Lambda.
   Continue::
   (onError errorStop)
   (setq filterFields (new Dictionary:))
   (assertRules)
   ;; Break the command up into its sub-commands.
   (setq cmds (stringToVector aSpec ";"))
   ;; Perform recursive rule substitution.
   (setq cmds (replaceRules cmds)) 
   ;; Establish the filter names array.
   (loop for i from 0 until (length cmds) do
       ;; Establish the filter names for each command.
       (setq cmdVector (pairToVector #void (lisp cmds[i] arithmetic:)))
       (loop for j from 0 until (length cmdVector) do
           (buildName cmdVector[j] "x")
           ) ;; end j loop
       ) ;end i loop
   ;; Add the filter names array as additional restrictions (if "nocheck" command not present).
   (if (and (> (length cmds) 0) (<> (trim cmds[0]) "nocheck"))
       (begin
          (loop for i from 0 until (length filterFields) do
             (if (<> filterFields[i 1] isDictionary:)
                 (begin
                    (if (<> filterFields[i 1] #void)
                        (begin
                           (setq aCmd (append "all (" filterFields[i 1] " |" filterFields[i 0] "|)"))
                           (setq cmds (vectorInsert cmds 0 aCmd))
                           ) ; end field type is not #void
                        (begin
                           (setq aCmd (append "all (<> " filterFields[i 0] " #void)"))
                           (setq cmds (vectorInsert cmds 0 aCmd))
                           )) ; end field type is #void
                    ))) ;; end loop
          )) ; end if
   ;; Perform rule merging.
   (setq cmds (mergeRules cmds))
   ;; Parse each sub command and append to the specification.
   (setq theSpec "")
   (loop for i from 0 until (length cmds) do
       ;; Call the proper command parser (based on the command word).
       (setq cmdVector (pairToVector #void (lisp cmds[i] arithmetic:)))
       (cond 
           ((= cmdVector[0] "all") (setq scmd (cmdAll cmdVector cmds[i]))) 
           ((= cmdVector[0] "check") (setq scmd ""))
           ((= cmdVector[0] "nocheck") (setq scmd ""))
           ((= cmdVector[0] "bottom") (setq scmd (cmdSort cmdVector cmds[i])))
           ((= cmdVector[0] "omit") (setq scmd (cmdOmit cmdVector cmds[i]))) 
           ((= cmdVector[0] "top") (setq scmd (cmdSort cmdVector cmds[i])))
           (else (error "compileFilter" cmds[i]))
           ) ; end cond
       (setq theSpec (append theSpec scmd _eol "   "))
       ) ;end loop
   ;; Convert the screen specification into a procedure.
   (setq proc (append "(lambda(TBC)" _eol "   "))
   (setq proc (append proc ";; TBC must be a data mine table cursor object." _eol "   "))
   (if (<> theSpec "")
       (setq proc (append proc theSpec)))
   (setq proc (append proc "true)"))
   (if (= debugOn true) (writeln proc)) ;; Use for testing only
   (setq proc (eval proc))
   (if (not (isLambda proc)) (error "compileFilter"))
   proc) ;; end compileFilter

;;  ***NOTES***:
;; 
;; see the screenBuilder notes, this Lambda implements the same query language.






















;;**EXPORTKEY**:dataMineLib:dmTableCursor:sort
(defChildLambda dataMineLib.dmTableCursor sort(sortLambda valueLambda sortOp)
;; *******************************************************************
;; summary:  Sort the entire table using a lambda sort Lambda.
;; Notes:    This algorithm will increase the sort cluster factor
;;           during the critical initial sort phase (to match the
;;           available RAM memory). 
;; Args:     sortLambda    Sort lambda(x y) value for record comparison.
;;           valueLambda   Value lambda(x) for record value extraction.
;;           sortOp       Sort operator: >=, <=, >, or <.
;; Return:   true        
;; *******************************************************************
    pvars:(;; Private Variable List
           (__testSW false)    ;; If test diagnostics are to be displayed, true; otherwise, false.
           __logFile           ;; Log file for testing purposes.
           ;; Private Methods List
           __sortReadBigBlock  ;; Read many smaller blocks into the UPDcurrentBlock, which becomes a giant block for sorting.
           __sortWriteBigBlock ;; Break the UPDcurrentBlock up into blocks of the correct blocking factor, then write each smaller block to the database.
           __testLog           ;; Read each record in the table and write contents to the test log.
           __testField         ;; Test field in each record to be written to the test log.
           ) ;; end of persistent variables
    vars:(blockLists           ;; Vector of block lists for each merge pass
          clusterIndex         ;; Index for scanning clustered blocks
          clusterSize          ;; Size of clustered blocks (always a log of 2)
          endTime              ;; Ending time for sort
          freeSpace            ;; Number of bytes of free space
          listCount            ;; Number of block list to create for each merge pass
          listIndex            ;; Index of the current block list to create
          mergeIndex           ;; Index of the current block to merge
          mergePassIndex       ;; Index of the current merge pass
          mergePassCount       ;; Number of merge passes required for this sort
          mergePassStart       ;; Starting value of the merge pass index
          mergeStepSize        ;; Number of blocks in each merge pass block list pair
          mergeRunCount        ;; Number of blocks in each merge pass block list
          recordOne            ;; Record one for merge
          recordTwo            ;; Record two for merge
          startTime            ;; Starting time for sort
          ) ;; end temporary variables
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    ;(if (<> __sortReadBigBlock #void) (goto Continue:))
    (defun __sortReadBigBlock(blockIndex clusterSize blockCount)
        ;; Read many smaller blocks into the UPDcurrentBlock,
        ;; which becomes a giant block for sorting.
        ;; Note: We have already read the first block 
        ;;       into UPDcurrentBlock.
        vars:(newBlock clusterIndex stopIndex)
        (setq stopIndex (min clusterSize (subi blockCount blockIndex)))
        (loop for clusterIndex from 1 until stopIndex do
            (setq newBlock (__readBlock newBlockRoot (addi blockIndex clusterIndex) false))
            (if __testSW 
                (begin 
                   (gc) 
                   (fwriteln __logFile "Read block [" (addi blockIndex clusterIndex) "] size = [" (sizeof newBlock) "] free = [" (inspect))  
                )) ;; end diagnostic if
            (setq UPDcurrentBlock (append UPDcurrentBlock newBlock))
            ) ;; end loop
        true) ;; end of __sortReadBigBlock
    (defun __sortWriteBigBlock(blockIndex)
        ;; Break the UPDcurrentBlock up into blocks of the correct blocking
        ;; factor, then write each smaller block to the database.
        vars:(newBlock clusterIndex clusterSize recordIndex blockStart blockLen)
        (setq clusterSize (ceiling (/ (length UPDcurrentBlock) blockSize)))
        (if (= clusterSize 1) (return (__writeBlock newBlockRoot blockIndex UPDcurrentBlock)))
        (loop for clusterIndex from 0 until clusterSize do
            (setq blockStart (muli clusterIndex blockSize))
            (setq newBlock (new Vector: object: (min blockSize (subi (length UPDcurrentBlock) blockStart))))
            (setq blockLen (length newBlock))
            (loop for recordIndex from 0 until blockLen do
                (setq newBlock[recordIndex] UPDcurrentBlock[(addi recordIndex blockStart)])
                ) ;; end recordIndex loop
            (__writeBlock newBlockRoot (addi blockIndex clusterIndex) newBlock)
            ) ;; end clusterIndex loop
        true) ;; end of __sortWriteBigBlock
    (defun __testLog()
        ;; Read each record in the table and write contents to the test log.
        vars:(recordIndex theRecord)
        (loop for recordIndex from 0 until recordCount do
            (setq theRecord (__updateRead recordIndex))
            (fwriteln __logFile "rec[" recordIndex "]." __testField " = " theRecord[__testField])  
            ) ;; end loop
        (__updateEnd)) ;; end of __testLog
    ;; Initialize the Lambda when called by new ObjectRepository: or attachLibrarian. 
    ;; This function always reattaches itself to the Repository (if necessary),
    ;; and always reestablishes the table block count, record count, and other statistics.
    Continue::
    ;; If this is a view, then resynchronize the view with the table prior to sorting.
    (if (= myObjectType view:) (setq sortLambda (__resynchSort valueLambda sortOp)))
    ;; Sort the records in each block.
    ;; Note: This is the first part of a merge  
    ;;       sort of the entire table database.
    (if __testSW (setq __logFile (fileOpen "sortlog.txt" 1 0)))
    (setq startTime (getTickCount 0))
    (gc)
    (setq freeSpace (- (inspect) 10000000))
    (if (isNegative freeSpace) (error "sort" "Insufficient workspace memory to perform a data mine table sort."))
    (setq clusterSize 1)
    (loop for UPDblockNUM from 0 until blockCount do
        (setq UPDcurrentBlock (__readBlock newBlockRoot UPDblockNUM false))
        ;; Compute the cluster size (if this is the first block)
        (if (and (= UPDblockNUM 0) (isPositive freeSpace))
            (begin
               (setq clusterSize (divi freeSpace (sizeof UPDcurrentBlock) 4))
               (setq clusterSize (expt 2 (integer (log2 clusterSize))))
               ;; Keep cluster size from growing too large and causing out of memory errors.
               ;; Note: This bug has been added to the defect tracking log and will be fixed later.
               ;(writeln "clusterSize = [" clusterSize "]")
               (if __testSW (fwriteln __logFile "Cluster size = [" clusterSize "]"))
               )) ;; end if
        ;; Read and sort blocks in clusters according to cluster size
        (__sortReadBigBlock UPDblockNUM clusterSize blockCount)
        (^sort UPDcurrentBlock sortLambda)
        (__sortWriteBigBlock UPDblockNUM)
        ;; Adjust block index for size of cluster
        (setq UPDblockNUM (sub1 (addi UPDblockNUM clusterSize)))
        ) ;; end of loop
    (setq UPDcurrentBlock #void)
    (setq UPDblockNUM #void)
    (setq UPDblockIDX #void)
    (setq UPDblockEXT #void)
    (setq UPDblockKEY #void)
    (if __testSW 
        (begin
           (fwriteln __logFile "Completed first pass sort.")
           (__testLog)
           )) ;; end test if
    ;; Perform the merge phase of the sort.
    ;; Note:  The number of merge passes is always equal
    ;;        to the log 2 of the number of blocks.
    ;; For Example:
    ;;        The merge phase of the sort for a database
    ;;        of seven blocks would appear as follows.
    ;;        (__merge2BlockLists (makeVector 1 0) (makeVector 1 1) sortLambda)
    ;;        (__merge2BlockLists (makeVector 1 2) (makeVector 1 3) sortLambda)
    ;;        (__merge2BlockLists (makeVector 1 4) (makeVector 1 5) sortLambda)
    ;;        (__merge2BlockLists (makeVector 1 6) (makeVector 0 0) sortLambda)
    ;;        (__merge2BlockLists (makeVector 2 0 1) (makeVector 2 2 3) sortLambda)
    ;;        (__merge2BlockLists (makeVector 2 4 5) (makeVector 1 6) sortLambda)
    ;;        (__merge2BlockLists (makeVector 4 0 1 2 3) (makeVector 3 4 5 6) sortLambda)
    (if (< clusterSize blockCount)
        (begin
           (setq mergePassCount (integer (log2 blockCount)))
           (setq mergePassStart (integer (log2 clusterSize)))
           (setq listCount (expt 2 mergePassStart))
           (if (<> mergePassCount (log2 blockCount)) (++ mergePassCount))
           (setq blockLists (new Vector: listCount))
           (setq mergePassIndex mergePassStart)
           (while (< mergePassIndex mergePassCount) do
               (if __testSW (fwriteln __logFile "Starting merge pass [" mergePassIndex "] of [" mergePassCount "]"))
               (setq mergeStepSize (expt 2 (add1 mergePassIndex)))
               (setq mergeRunCount (divi mergeStepSize 2))
               (setq mergeIndex 0)
               (__renameTmpBlocks)
               (while (< mergeIndex blockCount)
                   (setq listIndex 0)
                   (while (< listIndex listCount) do
                      (setq blockLists[listIndex] (__makeBlockList mergeIndex mergeRunCount blockCount))
                      (+= mergeIndex mergeRunCount)
                      (++ listIndex)
                      ) ;; end block list creation loop
                   ;; Write to test log (if necessary)..
                   (__mergeMBlockLists sortLambda blockLists)
                   ) ;; end merge stepping while
               (if __testSW 
                   (begin
                      (fwriteln __logFile "Completed merge pass [" mergePassIndex "] of [" mergePassCount "]")
                      (__testLog)
                      )) ;; end test if
               (+= mergePassIndex mergePassStart)
               ) ;; end merge passes loop
           )) ;; end merge if
    (setq rowOrderSW false)
    (setq lastOperation #void)
    (setq myIndex[myObjectName] (objectToStructure mySchema myParent.Pv))
    (setq endTime (integer (getTickCount startTime)))
    (if __testSW
        (begin
           (fwriteln __logFile "Records sorted = " recordCount " in " endTime " Seconds" )
           (setq __logFile (fileClose __logFile 1))
           )) ;; end test if
    true) ;; end of sort






















;;**EXPORTKEY**:dataMineLib:dmTableCursor:truncate
(defChildLambda dataMineLib.dmTableCursor truncate(selectLambda)
;; *******************************************************************
;; summary:  Truncate table to only those records where the selectLambda
;;           predicate returns true.
;; Args:     selectLambda   Select lambda predicate for selecting records.
;; Return:   recordCount   The number of records selected.     
;; *******************************************************************
    vars:(blockList            ;; Block list for 
          record               ;; Record to test against the selectLambda
          index                ;; Index of record to test against
          ) ;; end temporary variables
    ;; If the selectLambda is a number, then call truncateMid.
    (if (isNumber selectLambda) (return (__truncateMid selectLambda 0)))
    ;; Select only the records which match the select Lambda.
    ;; Note: The old blocks are renamed and deleted
    ;;       as they are read. Only the matched 
    ;;       records are kept.
    (__updateEnd)
    (setq blockList (__makeBlockList 0 blockCount blockCount))
    (__renameTmpBlocks)
    (__resetRecords)
    (if (= myObjectType table:)
        ;; Truncating a table
        (begin
           ;; Read each of the old records and keep the ones which
           ;; the selectLambda selects. The kept records are written
           ;; back to the table, and the others are disgarded.
           (setq record (__readBlockList blockList))
           (while (<> record #void)
               (if (selectLambda record) (__updateWrite recordCount record))
               (setq record (__readBlockList blockList))
               ) ;; end while
           ) ; end truncate a table if
        ;; Truncating a view
        (begin
           ;; Read each of the old records and keep the ones which
           ;; the selectLambda selects. The kept records are written
           ;; back to the table, and the others are disgarded.
           (if (<> rowOrderSW true) (__sortByRow))
           (myTableCursor.__updateEnd)
           (setq index (__readBlockList blockList))
           (if (<> index #void) (setq record (myTableCursor.updateRead index.__row)))
           (while (<> index #void)
               (if (selectLambda record) (__updateWrite recordCount index))
               (setq index (__readBlockList blockList))
               (if (<> index #void) (setq record (myTableCursor.updateRead index.__row)))
               ) ;; end while
           )) ; end truncate a table if
    (__updateEnd)
    recordCount) ;; end of truncate






















;;**EXPORTKEY**:dataMineLib:dropMinerChild
(defChildLambda dataMineLib:dropMinerChild(minerName option objType objName ...)
   ;; *******************************************************************
   ;; summary:  Deletes the specified objects from the specified miner in the data mine.
   ;;           
   ;;           
   ;;
   ;; Args:     minerName      Name of the miner from which to delete objects
   ;;           option         If datamine:, drop object from entire datamine.
   ;;           objType        Type of the object to be dropped    
   ;;           objName        Name of object to be dropped
   ;;           arg4           optional project name
   ;;           
   ;;
   ;; Return:   true
   ;;               
   ;; *******************************************************************
   ;;
   vars:(projName)
   ;;
   ;; Test for project
   (if (= (argCount) 5) 
      (if (<> (openProject (setq projName (argFetch 4))) true) 
         (return #void)
      )
   )
   (projectLambda.deleteMinerObject minerName objType objName projName)) ;; end of dropMinerObject













;;**EXPORTKEY**:dataMineLib:dropObject
(defChildLambda dataMineLib:dropObject(...)
    ;; *******************************************************************
    ;; summary:  Drop the specified object from the datamine.
    ;; Args:     ...      one or more object names to be dropped.
    ;; Return:   true
    ;; *******************************************************************
    vars:(indexOf cursor objectName)
    ;; Collect any object names.
    (loop for indexOf from 0 until (argCount) do
        (setq objectName (symbol (argFetch indexOf)))
        ;; Is the object a primary table?
        (if (or (<> primarySchema[objectName] #void) (<> blackboardSchema[objectName] #void))
            (begin
               (setq cursor (updateBegin objectName))
               (cursor.__updateKill)
               (updateEnd cursor)
            )) ;; end of primary table delete
        ) ;; end of object name loop
    true) ;; end of dropObject













;;**EXPORTKEY**:dataMineLib:dropOffLimits
(defChildLambda dataMineLib:dropOffLimits(goalName fieldName ...)
    ;; *******************************************************************
    ;; summary:  Drops the specified field from the specified goal Lambda's
    ;;           offlimits field list.
    ;;
    ;; Args:     goalName      name of the receiver object. 
    ;;           fieldName     name of the field to drop from the offlimits list
    ;;           projectName   name of project containing goal Lambda
    ;;           
    ;;
    ;; Return:   True
    ;;
    ;; *******************************************************************
    ;;
    ;;
    (if (= (argCount) 3)
       ;; Project supplied
       (if (<> (openProject projName) true) (return #void))
    )
    (measureCriteriaLambda.deleteExcludeName goalName fieldName)
    true) ;; end of dropOffLimits













;;**EXPORTKEY**:dataMineLib:dropProjectChild
(defChildLambda dataMineLib:dropProjectChild(option objType objName projName)
   ;; *******************************************************************
   ;; summary:  Deletes the specified objects from the specified project in the data mine.
   ;;           
   ;;           
   ;;
   ;; Args:     option         If datamine:, drop from datamine. 
   ;;           objType        Type of object to drop
   ;;           objName        Name of object to drop
   ;;           projName       Name project
   ;;           
   ;;
   ;; Return:   true
   ;;               
   ;; *******************************************************************
   ;;
   ;; Test for project
   (if (<> #void projName) (if (<> (openProject projName) true) (return #void)))
   ;;
   (projectLambda.deleteProjectObject projectLambda.Pv.currentProject[name:] objType objName)) ;; end of dropProjectChild













;;**EXPORTKEY**:dataMineLib:dropTableChild
(defChildLambda dataMineLib:dropTableChild(objType objName ...) 
    ;; *******************************************************************
    ;; summary:  Drops the specified object from the child objects
    ;;           associated with the specified table.
    ;;
    ;; Args:     objType         type of object to drop
    ;;           objName         name of object to drop
    ;;           arg2            name of primary table. If not supplied,
    ;;                           table referenced by last openTable, or
    ;;                           table associated with project referenced
    ;;                           by the last openProject. Else, a
    ;;                           tableRequired error is raised.
    ;;
    ;; Return:   True
    ;;
    ;; *******************************************************************
    ;;
    ;;
    (cond ((or (= objType filterLambda:) (= objType filter:))
             (selectCriteriaLambda.deleteCriteria objName)
          )
          ((or (= objType scoreLambda:) (= objType scorer:) (= objType goal:) (= objType goalLambda:))
             (measureCriteriaLambda.deleteCriteria objName)
          )
          ((= objType view:) 
             (string 5)
          )
          ((= objType miner:) 
             ()
          )
          ((= objType project:) 
             (projectLambda.deleteProject objName)
          )
    )
    true) ;; end dropTableChild













;;**EXPORTKEY**:dataMineLib:getColumnCount
(defChildLambda dataMineLib:getColumnCount(tableName)
   ;; *******************************************************************
   ;; summary:  Returns the number of columns in the specified table.
   ;;           
   ;;
   ;; Args:     tableName      name of the table whose column count to report
   ;;           
   ;;
   ;; Return:   Number of columns
   ;;               
   ;; *******************************************************************
   vars:(result)
   ;;
   ;;
   (if (= tableName #void) (setq result (_LegacyDB.Pv.refColCount)))
   result) ;; end of getColumnCount













;;**EXPORTKEY**:dataMineLib:getColumnNames
(defChildLambda dataMineLib:getColumnNames(tableName tableType)
   ;; *******************************************************************
   ;; summary:  Returns a structure of column names for the specified table.
   ;;           
   ;;
   ;; Args:     tableName      name of the table whose columns to return
   ;;           tableType      optional symbol to specify view:
   ;;           
   ;;
   ;; Return:   Structure of columns
   ;;               
   ;; *******************************************************************
   vars:(result tempString colVector i)
   ;;
   ;;
   (setq result (new Structure:))
   (if (= tableType view:)
      (begin
         (if (<> #void tableName) (openView tableName))
         (setq tempString (historyViewer.getColumnHeaders 0 (historyViewer.Pv.refColCount)))
      )
   else
      (begin
         (if (<> #void tableName) (openTable tableName))
         (setq tempString (_LegacyDB.getColumnHeaders 0 (_LegacyDB.Pv.refColCount)))
      )  
   )
   (setq colVector (stringToVector tempString #\tab))
   (loop for i from 0 until (length colVector) do 
      (setq result[(symbol colVector[i])] colVector[i]) 
   )
   result) ;; end of getColumnNames













;;**EXPORTKEY**:dataMineLib:getColumnTypes
(defChildLambda dataMineLib:getColumnTypes(tableName subTableName)
   ;; *******************************************************************
   ;; summary:  Returns a structure of column names and types for the specified table.
   ;;           
   ;;
   ;; Args:     tableName      name of the table whose columns to return
   ;;           subTableName   name of nested table whose columns to return
   ;;           
   ;;
   ;; Return:   Structure of column names and types
   ;;               
   ;; *******************************************************************
   vars:(result colVector typeVector keyVector i colCount)
   ;;
   ;;
   ;; Test to see if table is specified
   (if (<> #void tableName) (openTable tableName))
   ;;
   (setq result (new Structure:))
   (if (= subTableName #void) 
      (begin
         (setq colVector (stringToVector (_LegacyDB.getColumnHeaders 0 (length (_LegacyDB.getColVector))) #\tab))
         (setq typeVector (stringToVector (_LegacyDB.fieldTypes) #\tab))
      )
   else
      (begin
         (setq colVector (stringToVector (_LegacyDB.fieldSubfields subTableName) #\tab))
         (setq typeVector (stringToVector (_LegacyDB.fieldSubtypes subTableName) #\tab))
         (setq keyVector (stringToVector (_LegacyDB.fieldKeys subTableName) #\tab))
         (setq result[keys:] (vectorToDelimitedString keyVector #\tab))
      )
   )
   (setq colCount (length colVector))
   (loop for i from 0 until colCount do 
      (setq result[(symbol colVector[i])] typeVector[i])
   )
   result) ;; end of getColumnTypes













;;**EXPORTKEY**:dataMineLib:getDelimitedCells
(defChildLambda dataMineLib:getDelimitedCells(tableName tableType startRow endRow startCol endCol)
   ;; *******************************************************************
   ;; summary:  Returns a tabbed string of values from a table
   ;;           
   ;;
   ;; Args:     tableName      Name of the table from which obtain values
   ;;           tableType      view: or primary:
   ;;           startRow       1-based number of the 1st row from which to obtain values
   ;;           endRow         1-based number of the last row from which to obtain values
   ;;           startCol       1-based number of the 1st column from which to obtain values
   ;;           endCol         1-based number of the last column from which to obtain values
   ;;           
   ;;           
   ;;
   ;; Return:   Tab-delimited string of cell values
   ;;               
   ;; *******************************************************************
   vars:(result)
   ;;
   ;;
   (if (= tableType view:)
      (setq result (historyViewer.delimitedCells startRow endRow startCol endCol))
   else
      (setq result (_LegacyDB.delimitedCells startRow endRow startCol endCol))
   )
   result) ;; end of getDelimitedCells













;;**EXPORTKEY**:dataMineLib:getDelimitedColumns
(defChildLambda dataMineLib:getDelimitedColumns(tableName)
   ;; *******************************************************************
   ;; summary:  Returns a tab-delimited string of column names from the 
   ;;           specified table.
   ;;
   ;; Args:     tableName      name of the table whose columns to return
   ;;           
   ;;
   ;; Return:   Tab-delimited string of column names
   ;;               
   ;; *******************************************************************

   ;;
   ;;
   (vectorToDelimitedString (dataMineLib.getColumnNames tableName) #\tab)) 
   ;; end of getDelimitedColumns













;;**EXPORTKEY**:dataMineLib:getDelimitedObjects
(defChildLambda dataMineLib:getDelimitedObjects(...)
   ;; *******************************************************************
   ;; summary:  Returns the names of objects currently in the data mine.
   ;;
   ;; Args:     none            If there are no args, all objects are returned
   ;;           arg1            Type of objects to return. Valid types are
   ;;                              primary:    tables of type primary
   ;;                              view:       tables of type view
   ;;                              Lambda:      all goal, factory and filter Lambdas
   ;;                              filter:     only filter Lambdas
   ;;                              goal:       only goal Lambdas
   ;;                              factory:    only factory Lambdas
   ;;                              miner:      only miners
   ;;                              project:    only projects
   ;;                              offlimits:  offlimits fields list
   ;;           arg2            Name of the project, miner or goal Lambda from 
   ;;                           which to get objects 
   ;;
   ;; Return:   tab-delimited string of object names
   ;;
   ;; *******************************************************************
   vars:(rString)
   ;;
   ;;
   (if (= (argCount) 0) 
       ;; Get all object names
       (setq rString (vectorToDelimitedString (dataMineLib.getObjectNames) #\tab))
   else
       ;; Evaluate args to map to appropriate name source
       (setq rString (cond ((= (argFetch 0) goal:) 
                               ;; Goal Lambdas
                               (measureCriteriaLambda.loadCriteriaName))
                           ((= (argFetch 0) filter:) 
                               (if (= (argCount) 1)
                                  ;; Filter Lambdas
                                  (selectCriteriaLambda.loadCriteriaName)
                               else
                                  ;; Sorted filter Lambdas
                                  (selectCriteriaLambda.loadCriteriaName (argFetch 1))))
                           ((= (argFetch 0) offlimits:) 
                               ;; Offlimits fields
                               (measureCriteriaLambda.loadExcludeNames (argFetch 1)))
                           (else (vectorToDelimitedString 
                                    (if (= (argCount) 1) 
                                        (dataMineLib.getObjectNames (argFetch 0)) 
                                    else
                                        (dataMineLib.getObjectNames (argFetch 0) (argFetch 1))
                                    ) #\tab)
                           )
                     )
       )
   )
   rString) ;; end of getDelimitedObjects













;;**EXPORTKEY**:dataMineLib:getDelimitedScores
(defChildLambda dataMineLib:getDelimitedScores(minerName filterName sortOrder sortBy)
   ;; *******************************************************************
   ;; summary:  Returns the sorted, tab-delimited scores in the specified miner.
   ;;           
   ;;
   ;; Args:     minerName         Miner whose tab-delimited scores are returned
   ;;           filterName        Name of filter Lambda to score, or all: to score all FAs
   ;;           sortOrder         desc: for descending sort, asc: for ascending sort
   ;;           sortBy            avg:, sum:, min:, max:, std:, rar:, num: or none:
   ;;           
   ;;
   ;; Return:   Tab-delimited string of sorted scores
   ;;               
   ;; *******************************************************************
   ;;
   ;;
   (append "188328" #\tab "28473773" #\tab "714644408")) ;; end of getDelimitedScores













;;**EXPORTKEY**:dataMineLib:getMinerChildTab
(defChildLambda dataMineLib:getMinerChildTab(minerName objType projName)
   ;; *******************************************************************
   ;; summary:  Get a tab-delimited list of miner child objects
   ;;
   ;; Args:     minerName     name of the miner to list objects from
   ;;           objType       type of objects to list. Valid types are
   ;;                               filterLambda:
   ;;                               goalLambda:
   ;;                               scoreLambda:
   ;;                               view:
   ;;                          
   ;;           projName      optional project name
   ;;
   ;; Return:   TDS of child object names
   ;;               
   ;; *******************************************************************
   ;;
   ;; Test for project
   (if (<> #void projName) 
      (if (<> (openProject projName) true) (return #void))
   )
   ;;
   (vectorToDelimitedString (projectLambda.getMinerObjectList minerName objType) #\tab)) ;; end getMinerChildTab












;;**EXPORTKEY**:dataMineLib:getMinerGoal
(defChildLambda dataMineLib:getMinerGoal(minerName ...)
   ;; *******************************************************************
   ;; summary:  Returns the names of objects currently in the data mine.
   ;;
   ;; Args:     minerName     The name of the miner whose goal structure to get
   ;;           arg1          Name of the project containing the miner. If not
   ;;                         supplied, then the open project is used. 
   ;;                           
   ;;
   ;; Return:   goal Lambda structure
   ;;
   ;; *******************************************************************
   vars:(rGoal recCount index)
   ;;
   ;;
   (cond ((> (argCount) 2) 
            ;; Extra arg error
            (error argList)
         )
         ((= (argCount) 2)  
            ;; Project was supplied
            (begin 
               (if (<> projectLambda.Pv.currentProject[name:] (argFetch 1)) 
                  ;; Open new project
                  (if (= (openProject (argFetch 1)) #void)
                      (return #void)
                  )
               )
            )
         )
   )
   (setq rGoal (makeStructure  name:       #void,
                               source:     #void,
                               offLimits   #void))
               
   ;; Use open project
   (if (= (setq rGoal[name:] (projectLambda.getMinerObjectList minerName goalLambda:)) #void) 
      (error {minerRead})
   else
      ;; Load offlimits fields from V1.5 API
      (if (= (setq rGoal[offLimits:] (measureCriteriaLambda.loadExcludeNames rGoal[name:])) #void)
         (error {minerRead})
      else
         (if (= (setq rGoal[source:] (checkout rGoal[name:] goal:)) #void) 
            (error {goalRead}) 
         )
      )
   )
   rGoal) ;; end of getMinerGoal













;;**EXPORTKEY**:dataMineLib:getObjectNames
(defChildLambda dataMineLib:getObjectNames(...)
   ;; *******************************************************************
   ;; summary:  Returns the names of objects currently in the data mine.
   ;;
   ;; Args:     none            If there are no args, all objects are returned
   ;;           arg1            Type of objects to return. Valid types are
   ;;                              primary:    tables of type primary
   ;;                              view:       tables of type view
   ;;                              Lambda:      all goal, factory and filter Lambdas
   ;;                              filter:     only filter Lambdas
   ;;                              goal:       only goal Lambdas
   ;;                              factory:    only factory Lambdas
   ;;                              miner:      only miners
   ;;                              project:    only projects
   ;;           arg2            Name of the project or miner from which to get objects 
   ;;           arg3            Type of arg2 project: or miner:
   ;;                           
   ;;
   ;; Return:   vector of object names
   ;;
   ;; *******************************************************************
   vars:(rVector recCount index)
   ;;
   ;;
   (cond ((= (argCount) 0) (setq rVector (makeStructure Obj1: "Obj1" Obj2: "Obj2" Obj3: "Obj3")))
         ((= (argFetch 0) view:) 
             (begin 
                (setq recCount (length historyLambda))
                (setq rVector (new Vector: 0))
                (loop for index from 0 until recCount do (setq rVector[index] (string historyLambda[index 0])))
             ))
         ((= (argFetch 0) primary:) 
             (begin 
                (setq recCount (length _legacyDatabaseNames))
                (setq rVector (new Vector: 0))
                (loop for index from 0 until recCount do (setq rVector[index] (string _legacyDatabaseNames[index 0])))
             ))
         ((= (argFetch 0) filter:) 
             (if (= (argFetch 2) project:) 
                (setq rVector (projectLambda.getObjectList (argFetch 1) filter:))
             else
                (setq rVector (projectLambda.getObjectList (projectLambda.pv.currentProject.name) filter: (argFetch 1)))
             )
           )
         ((= (argFetch 0) project:) (setq rVector (projectLambda.getProjectList)))
         ((= (argFetch 0) miner:) (setq rVector (projectLambda.getObjectList (argFetch 1) miner:)))
   )
   rVector) ;; end of getObjectNames













;;**EXPORTKEY**:dataMineLib:getProjectChildTab
(defChildLambda dataMineLib:getProjectChildTab(objType projName)
   ;; *******************************************************************
   ;; summary:  Get a tab-delimited list of project child objects
   ;;
   ;; Args:     objType       type of objects to list. Valid types are
   ;;                               filterLambda:
   ;;                               scoreLambda:
   ;;                               miner:
   ;;                               view:
   ;;                          
   ;;           projName      optional project name
   ;;
   ;; Return:   TDS of child object names
   ;;               
   ;; *******************************************************************
   ;;
   ;; Test for project
   (if (<> #void projName) 
      (if (<> (openProject projName) true) (return #void))
   )
   ;;
   (vectorToDelimitedString (projectLambda.getObjectList projectLambda.Pv.currentProject[name:] objType) #\tab)) ;; end getProjectChildTab












;;**EXPORTKEY**:dataMineLib:getProjectMiner
(defChildLambda dataMineLib:getProjectMiner(minerName projName)
   ;; *******************************************************************
   ;; summary:  Returns the named miner structure
   ;;
   ;; Args:     minerName     name of miner structure to get
   ;;                          
   ;;           projName      optional project name
   ;;
   ;; Return:   TDS of child object names
   ;;               
   ;; *******************************************************************
   ;;
   ;; Test for project
   (if (<> #void projName) 
      (if (<> (openProject projName) true) (return #void))
   )
   ;;
   (projectLambda.getMiner minerName)) ;; end getProjectMiner












;;**EXPORTKEY**:dataMineLib:getScoredFilters
(defChildLambda dataMineLib:getScoredFilters(minerName scoreName)
   ;; *******************************************************************
   ;; summary:  Returns the sorted, tab-delimited scores in the specified miner.
   ;;           
   ;;
   ;; Args:     minerName         Miner whose tab-delimited scores are returned
   ;;           scoreName         Name of goal Lambda 
   ;;           
   ;;
   ;; Return:   Tab-delimited string of sorted filters with scores
   ;;               
   ;; *******************************************************************
   ;;
   ;;
   vars:(filterList resultString tempVector resultVector i n j recCount)
   ;;
   ;; get filter Lambda list, return void if empty
   (if (= minerName #void) 
      ;; Project-level filter folder
      (setq filterList (projectLambda.getObjectList projectLambda.Pv.currentProject.name filterLambda:))
   else
      ;; Miner-level filter folder
      (setq filterList (projectLambda.getMinerObjectList minerName filterLambda:))
   )
   (if (or (= filterList #void) (= filterList[0] #void)) (return #void))
    
   ;; Initialize record count and result variable
   (setq recCount (length filterList))
   (setq resultString "")
   ;; Build string of results
   (loop for i from 0 until recCount do
      (begin
         (setq tempVector (stringToVector (substitute (segmentLambda.delimitedScores {" & scoreName & "} {" & filterList[i] & "}) _eol #\tab) #\tab))
         (setq j 0)
         (setq resultVector (new Vector: 0))
         (loop for n from 1 to 13 by 2 do
            (begin 
               (setq resultVector[j] tempVector[n])
               (++ j)
            )
         ) 
         (setq resultString (append resultString filterList[i] #\tab (vectorToDelimitedString resultVector #\tab) _eol))
      )
   )
   resultString) ;; end of getDelimitedScores













;;**EXPORTKEY**:dataMineLib:getTableChildTab
(defChildLambda dataMineLib:getTableChildTab(objectType tableName)
   ;; *******************************************************************
   ;; summary:  Get a tab-delimited list of table child objects
   ;;
   ;; Args:     objectType       type of objects to list. Valid types are
   ;;
   ;;                               filterLambda:
   ;;                               scoreLambda:
   ;;                               miner:
   ;;                               project:
   ;;                               column:
   ;;                               view:
   ;;                          
   ;;           tableName        optional table name
   ;;
   ;; Return:   TDS of child object names
   ;;               
   ;; *******************************************************************
   vars:(result)
   ;;
   ;; Test for table
   (if (<> #void tableName) 
      (if (<> (openTable tableName) true) (return #void))
   )
   ;;
   (cond ((= objectType column:)
            (setq result (_LegacyDB.getColumnHeaders 0 (length (_LegacyDB.getColVector))))
         )
         ((= objectType project:)
            (setq result (vectorToDelimitedString (projectLambda.getProjectList) #\tab))
         )
         ((= objectType view:)
            (setq result (vectorToDelimitedString (getObjectNames view:) #\tab))
         )
         ((or (= objectType goal:) (= objectType goalLambda:) (= objectType scorer:) (= objectType scoreLambda:))
            (setq result (measureCriteriaLambda.loadCriteriaName))
         )
         ((or (= objectType filter:) (= objectType filterLambda:))
            (setq result (selectCriteriaLambda.loadCriteriaName))
         )
   )
   result) ;; end getTableChildTab












;;**EXPORTKEY**:dataMineLib:getTableGoal
(defChildLambda dataMineLib:getTableGoal(goalName tableName)
   ;; *******************************************************************
   ;; summary:  Returns the goal structure
   ;;
   ;; Args:     goalName     The name of the miner whose goal structure to get
   ;;           tableName    Name of the table containing the goal. If not
   ;;                        supplied, then the open table is used. 
   ;;                           
   ;;
   ;; Return:   goal Lambda structure
   ;;
   ;; *******************************************************************
   vars:(rGoal recCount index)
   ;;
   ;;
   (if (<> #void tableName)  
      ;; Project was supplied
      (begin 
         (if (<> currentTable tableName) 
             ;; Open new table
             (if (= (dataMineLib.openTable tableName) #void)
                (return #void)
             )
          )
      )
   )
   (setq rGoal (makeStructure  name:       #void,
                               source:     #void,
                               offLimits   #void))
               
   (setq rGoal[name:] goalName) 
   ;;
   ;; Load offlimits fields from V1.5 API
   (if (= (setq rGoal[offLimits:] (measureCriteriaLambda.loadExcludeNames rGoal[name:])) #void)
      (return #void)
   else
      (if (= (setq rGoal[source:] (checkout rGoal[name:] goal:)) #void) 
         (return #void) 
      )
   )
   rGoal) ;; end of getTableGoal













;;**EXPORTKEY**:dataMineLib:getTableTypes
(defChildLambda dataMineLib:getTableTypes()
   ;; *******************************************************************
   ;; summary:  Returns a structure of table names and types in the data mine.
   ;;
   ;; Args:     none        
   ;;                           
   ;;
   ;; Return:   structure of object names
   ;;
   ;; *******************************************************************
   ;;
   ;;
   (makeStructure Customers: primary: ChicagoSample: view: WesternSample: view:)) ;; end of getTableTypes













;;**EXPORTKEY**:dataMineLib:getTableViews
(defChildLambda dataMineLib:getTableViews(tableName)
   ;; *******************************************************************
   ;; summary:  Returns the Structure of view names for the specified 
   ;;           primary table in the data mine. Each view is bound to
   ;;           the source string of its Filter Lambda.
   ;;
   ;; Args:     tableName    Table whose views are to be returned
   ;;           
   ;;           
   ;;                        
   ;; Return:   Structure of view names and filter sources
   ;;
   ;; *******************************************************************
   ;;
   ;;
   (makeStructure TblView1: {all City = "Los Angeles"} TblView2: {omit State = "CA"} TblView3: {all HHIncome >= 100000})) ;; end of getTableViews













;;**EXPORTKEY**:dataMineLib:getTablesTab
(defChildLambda dataMineLib:getTablesTab()
   ;; *******************************************************************
   ;; summary:  Gets a tab-delimited string of primary tables in the data mine
   ;;
   ;; Args:     none        
   ;;                           
   ;;
   ;; Return:   TDS of tables
   ;;
   ;; *******************************************************************
   ;;
   ;;
   (vectorToDelimitedString (dataMineLib.getObjectNames primary:) #\tab)) ;; end of getTablesTab













;;**EXPORTKEY**:dataMineLib:mergeTables
(defChildLambda dataMineLib:mergeTables(tableName1 tableName2 mergeField mergeLambda)
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
   vars:(cursor1 cursor2 record1 record2 index1 index2)
   (setq cursor1 (updateBegin tableName1))
   (setq cursor2 (updateBegin tableName2))
   (setq record1 (cursor1.updateRead (setq index1 0)))
   (setq record2 (cursor2.updateRead (setq index2 0)))
   (while (and (<> record1 #void) (<> record2 #void)) do
      (cond ;; If record match they must both be merged.
            ((= record1[mergeField] record2[mergeField])
             (begin
                (mergeLambda record1 record2)
                (cursor1.updateWrite index1 record1)
                (cursor2.updateWrite index2 record2)
                (setq record1 (cursor1.updateRead (++ index1)))
                (setq record2 (cursor2.updateRead (++ index2)))
                )) ; end merge case
            ;; If the first record is low, we must promote table one.
            ((< record1[mergeField] record2[mergeField])
             (setq record1 (cursor1.updateRead (++ index1)))
                ) ; end promote table one case
            ;; If the second record is low, we must promote table two.
            (else
             (setq record2 (cursor2.updateRead (++ index2)))
                ) ; end promote table two case
            ) ; end of merge cond
      ) ;; end merge while
   (updateEnd cursor1)
   (updateEnd cursor2)
   true) ;; end of mergeTables













;;**EXPORTKEY**:dataMineLib:openProject
(defChildLambda dataMineLib:openProject(projectName)
    ;; *******************************************************************
    ;; summary:  Opens the specified project and initializes project Lambda.
    ;;           This must be called before manipulating the project.
    ;;
    ;; Args:     projectName      name of the project to open
    ;;
    ;; Return:   True
    ;;
    ;; *******************************************************************
    ;;
    ;; Make sure the projectLambda is initialized
    (projectLambda)
    (projectLambda.getProject projectName)
    true) ;; eop openProject
    












;;**EXPORTKEY**:dataMineLib:openTable
(defChildLambda dataMineLib:openTable(tableName)
    ;; *******************************************************************
    ;; summary:  Opens the specified table.
    ;;
    ;; Args:     tableName      name of the table to open
    ;;
    ;; Return:   True
    ;;
    ;; *******************************************************************
    ;;
    ;;
    (setq currentTable tableName)
    (_LegacyDB.Pv.refRowCount)) ;; eop openTable
    












;;**EXPORTKEY**:dataMineLib:openView
(defChildLambda dataMineLib:openView(viewName tableName)
    ;; *******************************************************************
    ;; summary:  Opens the specified table.
    ;;
    ;; Args:     viewName      name of the view to open
    ;;           tableName     optional name of table containing view.
    ;;
    ;; Return:   True
    ;;
    ;; *******************************************************************
    ;;
    ;; Set tbale if provided
    (if (<> #void tableName) 
       (setq currentTable tableName)
    )
    (historyViewer.doClear)
    ;; Open the view in the history viewer
    (if (<> (isLambda (historyViewer viewName)) true) (error {openView}))
    ;; Initialize view to ensure a full dataset
    (historyViewer.findAll)
    (historyViewer.Pv.refRowCount)) ;; eop openView
    












;;**EXPORTKEY**:dataMineLib:scoreFilter
(defChildLambda dataMineLib:scoreFilter(minerName filterName)
   ;; *******************************************************************
   ;; summary:  Scores one or all filter Lambdas in the specified miner.
   ;;           
   ;;           
   ;;
   ;; Args:     minerName     The miner in which to resume backtesting
   ;;           filterName    The Filter Lambda to score against all testing
   ;;                         tables in the miner. If all:, all unscored
   ;;                         filter Lambdas are scored.
   ;;           
   ;;
   ;; Return:   true
   ;;
   ;; *******************************************************************

   ;;
   ;;
   true) ;; end of scoreFilter













;;**EXPORTKEY**:dataMineLib:setColumnTypes
(defChildLambda dataMineLib:setColumnTypes(tableName typeStructure)
   ;; *******************************************************************
   ;; summary:  Sets the column types for the specified table.
   ;;           
   ;;
   ;; Args:     tableName      name of the table whose column types to set
   ;;           typeStructure  structure of column names bound to types.
   ;;                          Valid types are:
   ;;                              Any:  
   ;;                              Number:  
   ;;                              Symbol:  
   ;;                              String:  
   ;;                              Date:  
   ;;                              Boolean:  
   ;;                              Vector:  
   ;;                              Dictionary object for subtable fields.
   ;;           
   ;;
   ;; Return:   true
   ;;               
   ;; *******************************************************************

   ;;
   ;;
   true) ;; end of setColumnTypes













;;**EXPORTKEY**:dataMineLib:setFactoryLambda
(defChildLambda  dataMineLib:setFactoryLambda(minerName LambdaName LambdaStatus)
   ;; *******************************************************************
   ;; summary:  Turns a factory Lambda on/off for the specified miner.
   ;;           Must have a project open.
   ;;           
   ;;
   ;; Args:     minerName     Miner whose fitness goal is to be set
   ;;           LambdaName     Factory Lambda to set status
   ;;                             multivariateRegression:
   ;;                             geneticProgramming:
   ;;                             ruleInduction:
   ;;           LambdaStatus   true or false
   ;;           
   ;;
   ;; Return:   true
   ;;
   ;; *******************************************************************
   ;;
   ;;
   vars:(minerObject)
   (setq minerObject (projectLambda.getMiner minerName))
   (if (= minerObject #void) (error "!unknownMiner!"))
   (if (= minerObject.factoryList #void) 
      (setq minerObject.factoryList (makeStructure (symbol LambdaName) LambdaStatus))
   else
      (setq minerObject.factoryList[LambdaName] LambdaStatus)
   )
   (projectLambda.updateMiner minerName minerObject)
   true) ;; end of setFitnessGoal













;;**EXPORTKEY**:dataMineLib:setFitnessGoal
(defChildLambda  dataMineLib:setFitnessGoal(minerName goalName fitnessBias fitnessStat)
   ;; *******************************************************************
   ;; summary:  Sets the fitness goal in the specified miner.
   ;;           
   ;;           
   ;;
   ;; Args:     minerName     Miner whose fitness goal is to be set
   ;;           goalName      Goal Lambda to be used as the fitness goal
   ;;           fitnessBias   low: if low scores are fitter, high: if high scores are fitter
   ;;           fitnessStat   The statistic upon which fitness is scored:
   ;;                         avg:, sum:, min:, max:, std:, rar: or num:
   ;;           
   ;;
   ;; Return:   true
   ;;
   ;; *******************************************************************
   ;;
   ;;
   vars:(thisMiner)
   ;;
   (if (<> (setq thisMiner (projectLambda.getMiner minerName)) #void)
      (begin 
         (setq thisMiner[goalLambda:] goalName)
         (setq thisMiner[scoreStat:] fitnessStat)
         (setq thisMiner[scoreBias:] fitnessBias)
         (projectLambda.updateMiner minerName thisMiner)
      )
   )
   true) ;; end of setFitnessGoal













;;**EXPORTKEY**:dataMineLib:sort
(defChildLambda dataMineLib:sort(tableName sortLambda valueLambda sortOp ...)
   ;; *******************************************************************
   ;; summary:  Sorts the specified table in the data mine according to 
   ;;           the specified order.
   ;;
   ;; Style 1 Args:   tableName     The table to be sorted
   ;;                 sortLambda     Lambda(x y) predicate that has two arguments and sorts a pair of rows
   ;;                 valueLambda    Lambda(x) extracts the sort value from a row
   ;;                 sortOp        Sort operator: >=, <=, >, or <
   ;;               
   ;; Style 2 Args:   tableName                    The table to be sorted
   ;;                 ‘(colName1 . sortOrder1)     Column.Type pair 1
   ;;                 ‘(colNameN . sortOrderN)     Column.Type pair N  
   ;;
   ;;                 NOTE: valid sortOrderN values are asc: and desc:
   ;;
   ;; Return:   true
   ;; *******************************************************************
   vars:(cursor)
   (if (not (isLambda sortLambda)) (error "sort" "Column name argument option not yet implemented."))
   (setq cursor (updateBegin tableName))
   (cursor.sort sortLambda valueLambda sortOp)
   (updateEnd cursor)
   true) ;; end of sort













;;**EXPORTKEY**:dataMineLib:sortToView
(defChildLambda dataMineLib:sortToView(tableName viewName ...)
   ;; *******************************************************************
   ;; summary:  Sorts the specified table in the data mine according to 
   ;;           the specified order, placing the results in a view table.
   ;;           The original table is left unchanged.  If an identicle view
   ;;           table already exists, then no sorting takes place.
   ;;           
   ;;
   ;; Style 1 Args:   tableName     The table to be sorted
   ;;                 viewName      The view to be created
   ;;                 sortLambda     lambda predicate that has two arguments 
   ;;                               and sorts a pair of rows
   ;;               
   ;; Style 2 Args:   tableName     The table to be sorted
   ;;                 viewName      The view to be created  
   ;;                 ‘(colName1 . sortOrder1)     Column.Type pair 1
   ;;                 ‘(colNameN . sortOrderN)     Column.Type pair N  
   ;;
   ;;                 NOTE: valid sortOrderN values are asc: and desc:
   ;;
   ;; Return:   true
   ;;
   ;; *******************************************************************
   ;;
   ;;
   true) ;; end of sortToView













;;**EXPORTKEY**:dataMineLib:startMining
(defChildLambda dataMineLib:startMining(minerName numberOfGens)
   ;; *******************************************************************
   ;; summary:  Resumes the backtesting process for the specified miner.
   ;;           
   ;;           
   ;;
   ;; Args:     minerName     The miner in which to resume backtesting
   ;;           numberOfGens  The number of new Filter Lambda generations to 
   ;;                         generate before halting. If numberOfGens is 
   ;;                         zero, or missing, Filter Lambda generation 
   ;;                         continues until the engine receives an interrupt.
   ;;           
   ;;
   ;; Return:   true
   ;;
   ;; *******************************************************************

   ;;
   ;;
   true) ;; end of startMining













;;**EXPORTKEY**:dataMineLib:truncate
(defChildLambda dataMineLib:truncate(tableName truncateLambda)
   ;; *******************************************************************
   ;; summary:  Truncates a set of rows from the specified table 
   ;;           in the data mine.
   ;;
   ;; Args:     tableName      Name of the table to truncate
   ;;           truncateLambda  lambda predicate that accepts one argument. 
   ;;                          A row is truncated whenever the predicate 
   ;;                          returns a value of false. 
   ;;           arg2           If view: the table is unchanged and the logical
   ;;                          result of the truncation operation is written to
   ;;                          a new view table with a system-generated name,
   ;;                          which is the return value. 
   ;;                          If a view name (such as MyView:), then the table 
   ;;                          is unchanged and the logical result of the truncation
   ;;                          is written to the new view name.
   ;;                          
   ;;
   ;; Return:   true or a view name
   ;;
   ;; *******************************************************************
   vars:(cursor)
   (setq cursor (updateBegin tableName))
   (cursor.truncate truncateLambda)
   (updateEnd cursor)
   true) ;; end of truncate













;;**EXPORTKEY**:dataMineLib:validateACL
(defChildLambda dataMineLib:validateACL(LambdaType LambdaSource)
   ;; *******************************************************************
   ;; summary:  Validates ACL source code 
   ;;           
   ;;
   ;; Args:     LambdaType      ACL type to validate
   ;;           LambdaSource    ACL code to validate
   ;;           
   ;;
   ;; Return:   True or False
   ;;               
   ;; *******************************************************************
   vars:(result)
   ;;
   (if (= LambdaType filter:)
      (setq result (screenBuilder LambdaSource))
      (setq result (measureBuilder LambdaSource))
   )
   ;;
   result) ;; end validateACL











;;**EXPORTKEY**:debugScript
(defun debugScript(source)
;; *******************************************************************
;; summary:  This procedure compiles and evaluates the specified
;;           script file with debug options on. Each (defun) or (define)
;;           in the specified source file is separately compiled
;;           with its own source only. This allows the disassemble
;;           procedure to display more relevant source for each
;;           procedure contained within the specified source.
;; args:     source       => The source file name to be compiled.   
;; *******************************************************************
    (evalDebugScript (readTextFile source)))


























;;**EXPORTKEY**:directory
(defun  directory(path)
;; *************************************************************************************
;; summary:  Uses the Microsoft NT dir command to return a vector of the files in
;;           the specified directory.
;; Args:     path:       The Microsoft NT path for the directory.
;; Return:   data:       A vector of file name strings.
;; *************************************************************************************
    vars:(command data)
    ;; Send the dir command to the Microsoft NT command interpreter.
    (setq command (append "dir " path " /L /B > console.lst"))
    (system command)
    ;; Read the data from the temporary file.
    (setq data (readTextFile "console.lst"))
    ;; Return the vector of filename strings.
    (system "del console.lst")
    (stringToVector data _eol))

























;;**EXPORTKEY**:dummyDbms
(defun dummyDbms(objRepository)
;; *******************************************************************
;; Summary:  Database Librarian Lambda which manages a simple dummy 
;;           database and controls the dummy database schema. The 
;; Args:     objRepository  The dummy database repository object.
;; Return:   self           The dummy database managers.
;; *******************************************************************
    ;; Persistent Variables
    pvars:(myOR                ;; The table object repository
           myIndex             ;; The index of the table object repository
           (myResident false)  ;; The Lambda librarian residence switch
           self                ;; The parent Lambda
           ;; Child Lambdas
           abortTrans          ;; Manage repository abort transaction
           beginTrans          ;; Manage repository begin transaction
           commitTrans         ;; Manage repository commit transaction
           doClear             ;; Clear the table database
           len                 ;; Return the number of records in the table
           ) ;; end of pvars
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> doClear #void) (goto Continue:))
    ;; Initialize the inline child Lambdas.
    (defun abortTrans() (abortTransaction myOR))
    (defun beginTrans() (beginTransaction myOR))
    (defun commitTrans() (commitTransaction myOR))
    (defun doClear() (clear myOR))
    (defun len() (length myIndex))
    ;; Initialize the Lambda when called by new ObjectRepository: or attachLibrarian. 
    ;; This function always reattaches itself to the Repository (if necessary),
    ;; and always reestablishes the table block count, record count, and other statistics.
    Continue::
    (setq self (myself))
    (setq myResident false)
    ;; Ressign the internal object repository properly.
    (cond ((= (type objRepository) Lambda:)
           (setq myOR objRepository.myOR))
          ((<> (type objRepository) ObjectRepository:)
           (error "dMgr:myOR"))
	  ((<> myOR objRepository)
           (setq myOR objRepository))
	   ) ;; end cond
    (if (<> (type myOR) ObjectRepository:) (error "dMgr:myOR"))
    ;; Reattach self as the repository librarian properly.
    (if (<> (refLibrarian myOR) self)
	(begin
        (attachLibrarian myOR self)
        (return self)
	)) ;; end if
    self) ;; end of dummyDbms




















;;**EXPORTKEY**:errWriteln
(defun errWriteln(...)
;; *******************************************************************
;; summary:  This procedure is inserted in place of writeln
;;           to record and trap all errors in a permanent log.
;; args:     ...        
;; *******************************************************************
   pvars:(errorLog         ;; The error log
          writelnSave      ;; Save the old writeln
          ;; Methods
          reset            ;; Reset the error log
          restore          ;; Restore the old writeln
          show             ;; Display the error log
         ) ;; end persistant variables
   (defun reset() 
      (if (= writelnSave #void) 
          (setq writelnSave writeln)) 
      (setq writeln errWriteln)
      (setq errorLog (new Vector: 0))
      ) ;; end reset
   (defun restore() 
      (if (<> writelnSave #void) 
          (setq writeln writelnSave)) 
      (setq errorLog #void)
      ) ;; end restore
   (defun show()
      vars:(i n) 
      (setq n (length errorLog))
      (loop for i from 0 until n do
          (writelnSave errorLog[i])
          ) ;; end loop
      ) ;; end show
   ;; Write the information to the error log
   vars:(errMsg i n)
   ;; Make sure the error log is properly initialized.
   (if (= errorLog #void) (reset))
   ;; Assemble the output message for the error log.
   (setq n (argCount))
   (setq errMsg "")
   (loop for i from 0 until n do
       (setq errMsg (append errMsg (argFetch i)))
       ) ;; end loop
   ;; Add the error message to the error log
   (setq errorLog[(length errorLog)] errMsg)
   (writelnSave errMsg)
   true) ;; end errWriteln






























;;**EXPORTKEY**:evalDebug
(defun evalDebug(theText) 
;; *******************************************************************
;; summary:  This procedure compiles and evaluates the specified
;;           source with debug options on.
;; args:     source       => The source to be compiled.   
;; *******************************************************************
   vars:(proc lexresult morphresult)
   (debug true)
   (setq lexresult (_parser theText (setq proc (lambda() true)))) 
   (setq morphresult (morph lexresult))
   (compile morphresult proc)
   (debug false)
   (proc) proc)

























;;**EXPORTKEY**:evalDebugParts
(defun evalDebugParts(source)
;; *******************************************************************
;; summary:  This procedure compiles and evaluates the specified
;;           source with debug options on. Each (defun) or (define)
;;           in the specified source file is separately compiled
;;           with its own source only. This allows the disassemble
;;           procedure to display more relevant source for each
;;           procedure contained within the specified source.
;; args:     source       => The source to be compiled.   
;; *******************************************************************
    vars:(s start i)
    ;; Compile the whole source without debug first.
    (evalScript source)
    ;; Locate each balanced (defun) or (define ()) in the source.
    (loop for i from 0 until (length source) do
        (setq start (find "def" source i))
        (if (isNumber start)
            (begin
               (if (or (substringEQ source start (+ start 4) "defun" 0 4)
                       (substringEQ source start (+ start 5) "define" 0 5))
                   (begin
                       (setq s (balance source start))
                       (if (isString s)
                           (begin
                               (setq s (append s #\return))
                               (evalDebug s)
                               (setq i (+ start (length s) -1)))
                           (setq i start)))
                   (setq i start)))
            (setq i (length source)))))






























;;**EXPORTKEY**:evalScript
(defun evalScript(theText)
;; *******************************************************************
;; summary:  This procedure compiles and evaluates the specified
;;           source without debug options on.
;; args:     source       => The source to be compiled.   
;; *******************************************************************
   vars:(proc)
   (debug false)
   (setq proc (compile (morph (_parser theText))))
   (proc) 
   proc)






























;;**EXPORTKEY**:evolveGenebase
(defun  evolveGenebase(self ...)
;; *************************************************************************************
;; name:     evolveGenebase
;; 
;; summary:  Evolves the specified Genebase object, using a genetic algorithm, to search
;;           for individual Genomes which are good approximate solutions to the problem
;;           represented by its fitnessProc (see the makeGenebase procedure).
;; Args:     genebase             The Genebase to be evolved.
;;           continue             If present and if true, the Genebase will not be 
;;                                restarted from generation zero; but instead, will 
;;                                be restarted from the current generation.
;;           once                 If present and if true, the Genebase will not evaluate
;;                                multiple generations until reaching maxGenerations; 
;;                                but instead, will evaluate the next generation and 
;;                                then return.
;; Return:   self:                The Genebase which has been evolved.
;; *************************************************************************************
    vars:(continue once)
    ;; We must create the new Genebase's name
    (if (= name #void)
        (setq new-name (append "Genebase" (++ genebase-counter)))
        (setq new-name (string name)))
    ;; Fetch the two optional arguments.
    (if (>= (argCount) 2)
        (setq continue (argFetch 1))
        (setq continue false))
    (if (>= (argCount) 3)
        (setq once (argFetch 2))
        (setq once false))
    ;; Create the initial random population.
    (if (= self.generation 0)
        (self.birthProc self))
    ;; Return the evolved Genebase.
    self) ;; end of evolveGenebase




















;;**EXPORTKEY**:excludCriteriaLambda
(defun excludCriteriaLambda()
;; *******************************************************************
;;  Summary: This Lambda is in charge of History database exclusion
;;           Screening Strategies which exclude the unwanted
;;           selections.
;; Args:     none
;; Return:   myTView     The tableview of the exclusion criteria. 
;; *******************************************************************
    pvars:(excludST                   ;; The exclusion criteria Smarttable
           self                       ;; The parent Lambda object reference
           myTView                    ;; The exclusion criteria Tableview
           ;; Child Lambdas
           addNewCriteria             ;; Add the specified new criteria to the blackboard.
           Checkin
           deleteCriteria             ;; Delete the specified criteria from the blackboard.
           doClear
           findCriteriaName           ;; Return the selection criteria with the specified unique name. 
           findCriteriaSpecification  ;; Return the selection criteria with the specified unique Specification. 
           getColumnHeaders           ;; Return the History database valid column headers.
           getCriteria                ;; Return the selection criteria strategy specification string
           loadCondition 
           loadCriteriaName           ;; Load list of all criteria names for display in the names list box. 
           loadCriteriaNotes          ;; Return the selection criteria notes string
           loadFieldName 
           loadLogic 
           loadSort  
           loadValue
           ref2
           refColCount
           refColKeyness
           refColName
           refColType
           refRowCount
           renameCriteria             ;; Rename the specified criteria
           saveCriteriaNotes          ;; Save the specified criteria notes.
           set2
           ) ;; end of persistant variables
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> ref2 #void) (goto ReInitialize:))
    ;; Initialize the inline child Lambdas.
    (defun addNewCriteria(newSpec newName newNotes) 
    ;; Add the specified new criteria to the blackboard.
       vars:(rowIndex nameIndex)
       ;; If no user name, generate a new name automatically.
       (if (= newName #void)
           (setq newName (append "Strategy_" (now)))
           ) ;; end if
       ;; Load exclusion strategies table and set up long transaction.
       (beginTransaction _Blackboard) 
       (setq excludST (excludLoadStrategies))      
       ;; See if the exclusion criteria table is already full.
       (if (>= excludST.rowCount 1500)
           (begin
              (abortTransaction _Blackboard)
              (error "databaseFull")
           )) ;;end if
       ;; See if the user criteria specification is already in use.
       (setq rowIndex (findCriteriaSpecification newSpec))
       (if (isNumber rowIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "CriteriaInUse")
           )) ;;end if
       ;; See if the user criteria name is already in use.
       (setq nameIndex (findCriteriaName newName))
       (if (isNumber nameIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "NameInUse")
           )) ;;end if
       ;; Add the exclusion criteria.
       (setq rowIndex excludST.rowCount)
       (addRow excludST 1 rowIndex)
       (setq excludST[Specification: rowIndex] newSpec)
       (setq excludST[Notes: rowIndex] newNotes)
       (setq excludST[Name: rowIndex] newName)
       (setq _Blackboard.excludLambda excludST)
       (commitTransaction _Blackboard)
       newName) ;; end saveCriteriaNotes
    (defun Checkin(title) (writeln "Checked in " title))
    (defun deleteCriteria(criteriaName) 
    ;; Delete the specified criteria from the blackboard.
       vars:(rowIndex detailTableKey)
       (beginTransaction _Blackboard) 
       (setq excludST (excludLoadStrategies))      
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "NoOldName")
           )) ;;end if
       ;; Delete the selection criteria (if any).
       (deleteRow excludST 1 rowIndex)
       (setq _Blackboard.excludLambda excludST)
       (commitTransaction _Blackboard)
       true) ;; end saveCriteriaNotes
    (defun doClear() (setq excludST (new Smarttable:)))
    (defun findCriteriaName(name)
    ;; Return the selection criteria with the specified unique name. 
       vars: (rowIndex rowCount nameString)
       (if (= excludST #void) (excludCriteriaLambda)) ;; Make sure we're initialized.
       (setq nameString (string name))
       (loop for rowIndex from 0 until excludST.rowCount do
          (if (= nameString excludST[Name: rowIndex])
              (return rowIndex))
          ) ;; end loop
       false) ;; end findCriteriaName
    (defun findCriteriaSpecification(specString)
    ;; Return the selection criteria with the specified unique Specification. 
       vars: (rowIndex rowCount nameString)
       (if (= excludST #void) (excludCriteriaLambda)) ;; Make sure we're initialized.
       (loop for rowIndex from 0 until excludST.rowCount do
          (if (= specString excludST[Specification: rowIndex])
              (return rowIndex))
          ) ;; end loop
       false) ;; end findCriteriaSpecification
    (defun getColumnHeaders(startCol endCol) 
    ;; Return the History database valid column headers.
        vars:(i structLen dls cellValue)  
        ;;no value need as it is an RTTI
        (setq dls "")
        (setq structLen (ref (length _validFields)))
        (loop for i from 0 until structLen do
            (setq dls (append dls (ref _validFields [i 0]) (char 09)))
            ) ;;end i loop
        (append dls ""))
    (defun getCriteria(criteriaName)
    ;; Return the selection criteria strategy specification string
    ;; saved under the specified unique name.
       vars:(rowIndex result)
       (if (= excludST #void) (excludCriteriaLambda)) ;; Make sure we're initialized.
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (setq result "")
           (setq result excludST[Specification: rowIndex])
           ) ;;end if
       result) ;; end getCriteria
    (defun loadCondition() 
       vars: (conditionString)
       (setq conditionString "")
       (setq conditionString (append "and" (char 09) "or" (char 09) "<" (char 09) "<=" (char 09) "=" (char 09)))
       (setq conditionString (append conditionString ">" (char 09) ">=" (char 09) "<>" (char 09))))
    (defun loadCriteriaName()
    ;; Load list of all criteria names for display in the names list box. 
       vars: (rowIndex rowCount nameString)
       (if (= excludST #void) (excludCriteriaLambda)) ;; Make sure we're initialized.
       (setq nameString "")
       (loop for rowIndex from 0 until excludST.rowCount do
          (setq nameString (append nameString excludST[Name: rowIndex] (char 09)))
          ) ;; end loop
       nameString) ;; end loadCriteriaName
    (defun loadCriteriaNotes(criteriaName) 
    ;; Return the selection criteria notes string
    ;; saved under the specified unique name.
       vars:(rowIndex result) 
       (if (= excludST #void) (excludCriteriaLambda)) ;; Make sure we're initialized.
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (setq result "")
           (setq result excludST[Notes: rowIndex])
           ) ;;end if
       result) ;; end loadCriteriaNotes
    (defun loadFieldName() 
       vars: (columnHeaders)
       (setq columnHeaders (self.getColumnHeaders 0 (self.refColCount))))
    (defun loadLogic() (append "and" (char 09) "or" (char 09)))
    (defun loadSort() 
       vars: (sortString)
       (setq sortString "")
       (setq sortString (append  sortString "all" (char 09) "top" (char 09)))
       (setq sortString (append  sortString "bottom" (char 09) "omit" (char 09) "rule")))
    (defun loadValue() 
       vars: (valueString)
       (setq valueString ""))
    (defun ref2(col row) myTView[col row])
    (defun refColCount() myTView.colCount)
    (defun refColKeyness(col) myTView.colVector[col].colFormat)
    (defun refColName(col) myTView.colVector[col].name)
    (defun refColType(col) "G")
    (defun refRowCount() myTView.rowCount)
    (defun renameCriteria(oldName newName) 
    ;; Rename the specified criteria. Make sure the
    ;; new criteria name is unique.
       vars:(oldIndex newIndex)
       (beginTransaction _Blackboard) 
       (setq excludST (excludLoadStrategies))      
       (setq newIndex (findCriteriaName newName))
       (if (isNumber newIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "NameInUse")
           )) ;;end if
       (setq oldIndex (findCriteriaName oldName))
       (if (isBoolean oldIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "NoOldName")
           )) ;;end if
       (setq excludST[Name: oldIndex] (string newName))
       (setq _Blackboard.excludLambda excludST)
       (commitTransaction _Blackboard)
       newName) ;; end renameCriteria
    (defun saveCriteriaNotes(criteriaName newNotes) 
    ;; Save the specified criteria notes.
       vars:(rowIndex)
       (beginTransaction _Blackboard) 
       (setq excludST (excludLoadStrategies))      
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "NameInUse")
              (return false)
           )) ;;end if
       (setq excludST[Notes: rowIndex] (string newNotes))
       (setq _Blackboard.excludLambda excludST)
       (commitTransaction _Blackboard)
       true) ;; end saveCriteriaNotes
    (defun set2(col row newValue) (set myTView col row newValue))
    ;; Initialize the exclusion Lambda
    ReInitialize::
    (setq excludST (excludLoadStrategies))
    (setq self (myself))
    (setq myTView (new Tableview: excludST))
    ) ;; end of excludCriteriaLambda













;;**EXPORTKEY**:excludLoadStrategies
(defun excludLoadStrategies(...)
;; *******************************************************************
;; Summary:  Load the specified History Exclusion Strategy to the screen
;;           Lambdas list of strategies to be backtested. Each strategy 
;;           has a unique specification in the language defined by the 
;;           screenBuilder procedure.
;; Note:     We use the blackboard to store the results of backtesting
;;           for many different history database screening strategies.
;; Args:     strategy       (optional) Specification of strategy to add.
;; Return:   excludTotals   The new screen Lambdas list of strategies
;;                          for backtesting. 
;; *******************************************************************
   vars:(x y i n excludTotals key strategy)
   ;; Obtain the specification of the strategy to add.
   (if (>= (argCount) 1)
       (setq strategy (argFetch 0))
       (setq strategy #void))
   ;; The specifications for all history exclusion strategies back tested
   ;; so far, are stored in the blackboard under the key "excludLambda". This
   ;; should be a Smarttable of the "exclude" type (see screenResultTables). 
   (setq excludTotals _Blackboard.excludLambda)
   ;; If the blackboard is empty, then we must create an exclusion smarttable
   ;; and enter an initial history exclusion strategy for back testing. 
   (if (= excludTotals #void)
       (setq excludTotals (_defaultExcludStrategies))
       (setq _Blackboard.excludLambda excludTotals)
       ) ;; end if
   ;; If a strategy was specified, then add it.
   (if (<> strategy #void)
       (begin
          (setq key (new Structure: Specification: strategy))
          (updateRowByKey excludTotals key)
          ;; Write the screen Lambdas list of strategies back to the blackboard.
          (setq _Blackboard.excludLambda excludTotals)
          )) ;; end if
   excludTotals)































;;**EXPORTKEY**:exportLayoutLambda
(defun exportLayoutLambda()
;; *******************************************************************
;; summary:  Manages all export layout functions for the Predictive Modeler
;;           The record keys for the _ExportLayoutDB are as follows:
;;           
;;             
;; 
;; Args:     none
;; Return:   true
;; *******************************************************************
  pvars:(myParent                ;; Holds the parent Lambda object reference
         currentLayout           ;; Currently loaded layout record
         currentDirectory        ;; Currently loaded layout directory
         exportFieldData         ;; The current export field metadata dictionary 96.06.26 WJH
         exportLayouts           ;; The current export layout dictionary
         ;;methods
         addLayout               ;; Adds a layout record
         addField                ;; Adds a field to a layour record
         clearLayouts            ;; Removes all layouts
         createLayoutStructure   ;; Creates the structure used to store layout info
         deleteField             ;; Deletes a field from a layout record
         deleteLayout            ;; Deletes a layout record
         exportCells             ;; Return a set of values from the table database as a tab delimited string.
         getLayout               ;; Returns a particular layout record as a tab delimited string
         getLayoutList           ;; Returns list of layout names
         getLayoutRecord         ;; Returns a particular layout record
         initLayoutDirectory     ;; Initializes layout directory   
         saveLayoutDirectory     ;; Saves layout directory
         updateField             ;; Update a field in a layout record
         updateLayout            ;; Update a layout record  
         )   ;;end of persistant variables
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistent storage of the original Lambda.
    (if (<> addLayout #void) (goto Continue:))
    ;; Initialize the inline child Lambdas.
    (defun addLayout(layoutName layoutFile layoutType layoutTypeData layoutFields layoutData)
       ;; *******************************************************************
       ;; summary:  Add a new export layout. Create entry
       ;;           by parsing a tab delimited string.
       ;;
       ;; Args:     layoutName     name of export layout
       ;;           layoutFile     export filename
       ;;           layoutType     export as "delimited","fixed",etc. 
       ;;           layoutTypeData custom delimiter 
       ;;           layoutFields   tabbed list of field names to export
       ;;           layoutData     tabbed list of export-dependent metadata (length of field)
       ;;
       ;; Return:   True
       ;;               
       ;; *******************************************************************
       vars:(
          exportFields            ;; Field list vector
          exportData              ;; Field metadata vector
          thisField               ;; Field index
         ) 

       ;;
       ;;make sure Directory exists
       (initLayoutDirectory)
       ;;
       ;; Retrieve the export layout from the Dictionary to test for duplicates
       (if (<> (setq currentLayout currentDirectory[layoutName]) #void)
           (error "duplicateLayout"))
       ;;
       ;; Convert the tabbed lists to a vectors
       (if (<> layoutFields #void) 
          (setq exportFields (stringToVector layoutFields "#\tab"))    
       )
       (if (<> layoutData #void) 
          (begin
             (setq exportData (stringToVector layoutData "#\tab"))
             ;;
             ;; Create field metadata dictionary
             (setq exportFieldData (^new Dictionary:))
             ;;
             ;; Transfer data to new dictionary
             (loop for thisField from 0 until (length exportFields) do
                (setq exportFieldData[exportFields[thisField]] exportData[thisField])
             )
          )    
       )
       ;;
       ;; Create an empty layout record
       (createLayoutStructure)
       ;;
       ;; Initialize layout fields
       (setq currentLayout.name layoutName)
       (setq currentLayout.file layoutFile)
       (setq currentLayout.type layoutType)
       (setq currentLayout.typeData layoutTypeData)
       (setq currentLayout.fieldList exportFields)
       (setq currentLayout.fieldData exportFieldData)
       ;;
       ;; Save new export layout to the directory
       (setq currentDirectory[layoutName] currentLayout)
       ;;
       ;; Save the modified directory
       (saveLayoutDirectory)
       ;;
       true) ;; end of addLayout
    (defun addField(layoutName layoutField)
       ;; *******************************************************************
       ;; summary:  Add a new field to an export layout. 
       ;;
       ;; Args:     layoutName     name of export layout
       ;;           layoutField    field name to add
       ;;
       ;; Return:   True
       ;;               
       ;; *******************************************************************
       vars:(
             thisField               ;; Field index variable
            ) 
       ;;
       ;; Initialize the directory
       (initLayoutDirectory)
       ;;
       ;; Retrieve the export layout from the Dictionary
       (if (= (setq currentLayout currentDirectory[layoutName]) #void)
           (error "unknownLayout"))
       ;;
       ;; Initialize field index
       (setq thisField (length currentLayout.fieldList))
       ;;
       ;; Format field name
       (setq layoutField (trim layoutField))
       ;;
       ;; Add new layout field
       (setq currentLayout.fieldList[thisField] layoutField)
       ;;
       ;; Test for field data and initialize position 
       ;; to zero if fieldData present
       (if (<> currentLayout.fieldData #void) (setq currentLayout.fieldData[layoutField] 0))
       ;;
       ;; Save export layout to directory
       (setq currentDirectory[layoutName] currentLayout)
       (saveLayoutDirectory)
       true) ;; end of addField
    (defun clearLayouts()
       ;;clears the layout directory
       ;;
       ;;initialize layout directory
       (initLayoutDirectory)
       ;;erase current layout
       (createLayoutStructure)
       ;;erase layout directory
       (setq currentDirectory #void)
       ;;write to repository
       (saveLayoutDirectory)
       true) ;; eop clearLayouts
    (defun createLayoutStructure()
       (setq currentLayout (makeStructure name:        #void 
                                          file:        #void
                                          type:        #void 
                                          typeData:    #void
                                          fieldList:   #void 
                                          fieldData:   #void))
       true) ;; eop createLayoutStructure
    (defun deleteField(layoutName layoutField)
       ;; *******************************************************************
       ;; summary:  Delete a field from an export layout. 
       ;;
       ;; Args:     layoutName     name of export layout
       ;;           layoutField    field name to remove
       ;;
       ;; Return:   True
       ;;               
       ;; *******************************************************************
       vars:(
             thisField               ;; Field index variable
            ) 

       ;;
       ;; Initialize the directory
       (initLayoutDirectory)
       ;;
       ;; Retrieve the export layout from the Dictionary
       (if (= (setq currentLayout currentDirectory[layoutName]) #void)
           (error "unknownLayout"))
       ;;
       ;; Initialize field index
       (setq thisField (length currentLayout.fieldList))
       ;;
       ;; Test for existence of specified field
       (if (= (setq thisField (member layoutField currentLayout.fieldList)) false)
          (error "unknownField")
       )
       ;;
       ;; Test for existence of field metadata dictionary
       (if (<> currentLayout.fieldData #void)
          ;; Erase entry in dictionary
          (delete currentLayout.fieldData currentLayout.fieldList[thisField])
       )
       ;;
       ;; Erase field entry in field vector
       (delete currentLayout.fieldList thisField)
       ;;
       ;; Save export layout to dictionary
       (setq currentDirectory[layoutName] currentLayout)
       (saveLayoutDirectory)    
       ;;
       true) ;; end of deleteField
    (defun deleteLayout(layoutName) 
       ;;initialize layout directory
       (initLayoutDirectory)
       ;;attempt to load specified layout 
       (setq currentLayout currentDirectory[layoutName])
       ;;error if layout not found
       (if (= currentLayout #void)
          (error "UnknownExportLayout")
       )
       ;;void layout in directory and current layout 
       (setq currentDirectory[layoutName] #void)
       (setq currentLayout #void)
       (saveLayoutDirectory)
       ;; return true
       true) ;; eop deleteLayout
    (defun exportCells(formatName exportAs exportDelim fileName startRow endRow)
    ;; Return a set of values from the table database as a tab delimited string.
       vars:(
             cellValue               ;; Data field value from database
             dls                     ;; Row output buffer
             exportFormatRecord      ;; Export record layout format data
             fieldName               ;; Name of output field
             fieldLen                ;; Field output length
             fileId                  ;; File handle
             getRow                  ;; Row index
             layoutLength            ;; Length of layout data
             myPos                   ;; Delimiter position terminating field in layout data
             outputField             ;; Buffer for building fixed-length output field
             record                  ;; Record data
            ) 
       ;; Retrieve the export layout Dictionary from the repository index.
       (if (<> myIndex #void) 
          (setq exportLayout myIndex._exportLayout)
          (begin
             (beginTransaction myOR) 
             (setq exportLayout myIndex._exportLayout)
             (abortTransaction myOR)
          )
       )
    
       ;; Format exportAs parameter to avoid confusion
       (setq exportAs (upcase (left exportAs 3)))

       ;; Create the export layout Dictionary (if necessary).
       (if (= exportLayout #void) 
           (setq exportLayout (^new Dictionary:))
       )

       ;; Retrieve the export layout from the Dictionary.
       (if (= (setq exportFormatRecord exportLayout[formatName]) #void)
           (error "unknownFormat")
       )

       ;; Return a tab delimited string of cell values for
       ;; all cells between the following rows and columns.

       ;; Initialize row buffer
       (setq dls "")
     
       ;; Open file, getting handle
       (setq fileId (fileOpen  fileName 1 0))
     
       ;; For all rows by index, assuming user gives us 1-based record parameters
       ;; and further assuming that refRowseq is zero-based
       (if (= startRow 0) (setq startRow 1))

       (loop for getRow from (- startRow 1) until endRow do
          (beginTransaction myOR) 
            
          ;; Read database row
          (setq record (refRowSeq getRow))
                  
          ;; Initialize position variables
          (setq myPos 0)
          (setq layoutLength (length exportFormatRecord.fieldList))
                 
          ;; Output fields for this row
          (while (< myPos layoutLength) do
             ;; Parse out the export field name
             (setq fieldName exportFormatRecord.fieldList[myPos])
                                        
             ;; Store the field's value
             (setq cellValue record[(symbol fieldName)])
                 
             ;; Set #void values to an empty string -- 0 bytes will be output
             (if (= cellValue #void) then (setq cellValue ""))
                  
             ;; Format numeric field values
             (if (= (isString cellValue) false)
                (setq cellValue (text cellValue "#")))
                
             ;; Branch to fixed-length format -------------------------------
             (if (= exportAs "FIX") 
                (begin
                
                   ;; Store the length of the output field
                   (setq fieldLen exportFormatRecord.fieldData[exportFormatRecord.fieldList[myPos]])
                       
                   ;; Initialize the outputField to spaces
                   (setq outputField (rept " " fieldLen))
                        
                   ;; Truncate any excess bytes per the specified field len
                   (if (< fieldLen (length cellValue))
                      (setq cellValue (left cellValue fieldLen)))
                      
                   ;; Move the cell value into the output variable
                   ;; Truncate, if necessary
                   (if (< (length cellValue) fieldLen)
                      (setq outputField (replace outputField 0 fieldLen cellValue))
                      (setq outputField (replace outputField 0 (length cellValue) cellValue))
                   )
              
                   ;; Add the output field to the row buffer
                   (setq dls (append dls outputField))
                )
             )
          
             ;; Branch to Delimited ------------------------------------
             (if (= exportAs "DEL")  
                (begin
                   (setq dls (append dls cellValue))
                   (setq dls (append dls (char (number exportDelim))))
                )
             )

             ;; Increment position variable
             (++ myPos)

          ) ;; end while (< layoutLength myPos)
       
          ;; Output row to file               
          (fileWrite  fileId  (append dls _eol))
          ;; Commit transaction
          (commitTransaction myOR)
          ;; Clear row buffer
          (setq dls "") 
          ;; process next row
       ) ;;end getRow loop

       ;; Close the file
       (fileClose  fileId  1)

       ;; Save the export layout Dictionary to the repository index.
       (if (<> myIndex #void) 
           (setq myIndex._exportLayout exportLayout)
           (begin
              (beginTransaction myOR) 
              (setq myIndex._exportLayout exportLayout)
              (commitTransaction myOR)))
   
       true) ;; end of exportCells
    (defun getLayoutList() 
       ;; Return the list of layouts as a TDS
       vars: (i exportList recCount)
       (initLayoutDirectory)
       (setq recCount (length currentDirectory))
       (setq exportList "")
       (loop for i from 0 until recCount do
           (setq currentLayout currentDirectory[i 1])
           (setq exportList (append exportList currentLayout.name (char 09)))
           ) ;;end i loop
       exportList) ;; eop getLayoutList
    (defun getLayout(layoutName)
       ;; returns the record of a layout as a TDS
       vars: (
              exportString        ;; Output variable
              stringPos           ;; Position index
             )
       ;;
       ;; Assure directory object is initialized
       (initLayoutDirectory)
       ;;
       ;; Retrieve specified layout
       (if (= (setq currentLayout currentDirectory[layoutName]) #void)
          (error "unknownFormat"))
       ;;
       ;; Initialize output variable
       (setq exportString "")
       ;;
       ;; Append heading fields to output variable
       (setq exportString (append exportString currentLayout.name #\tab currentLayout.file #\tab currentLayout.type #\tab currentLayout.typeData #\tab))
       ;;
       ;; Test for non-empty field list
       (if (<> currentLayout.fieldList #void)
          (begin
             ;; Append field list to output variable
             (setq stringPos 0)
             (while (> (length currentLayout.fieldList) stringPos) do
                (setq exportString (append exportString currentLayout.fieldList[stringPos] (char 9)))
                (++ stringPos)
             ) 
             ;;
             ;; Test to see if field-level metadata was included
             (if (<> currentLayout.fieldData #void)
                (begin
                   ;; Append a token to denote the beginning of field metadata
                   (setq exportString (append exportString "|" (char 9)))
                   ;;
                   ;; Append metadata to output variable
                   (setq stringPos 0)
                   (while (> (length currentLayout.fieldData) stringPos) do
                      (setq exportString (append exportString currentLayout.fieldData[currentLayout.fieldList[stringPos]] (char 9)))
                      (++ stringPos)
                   ) 
                )
             )
          )
       )
       exportString) ;; eop getLayout
    (defun getLayoutRecord(layoutName)
       ;; returns the record of a layout
       ;;
       ;; Assure directory object is initialized
       (initLayoutDirectory)
       ;;
       ;; Retrieve specified layout
       (if (= (setq currentLayout currentDirectory[layoutName]) #void)
           (error "unknownFormat"))
       currentLayout) ;; eop getLayoutRecord
    (defun initLayoutDirectory()
       ;; initializes layout directory
       (if (= _ExportLayoutDB[layoutDirectory:] #void)
          (begin
             (beginTransaction _ExportLayoutDB)
             (setq _ExportLayoutDB[layoutDirectory:] (^new Directory:))
             (commitTransaction _ExportLayoutDB)
          )
       )
       (beginTransaction _ExportLayoutDB)
       (setq currentDirectory _ExportLayoutDB[layoutDirectory:])
       (abortTransaction _ExportLayoutDB)
       true) ;; eop initLayoutDirectory
    (defun saveLayoutDirectory()
       ;;saves layout directory
       (beginTransaction _ExportLayoutDB)
       (setq _ExportLayoutDB[layoutDirectory:] currentDirectory)
       (commitTransaction _ExportLayoutDB)
       true)
    (defun updateField(layoutName layoutField layoutData)
       ;; *******************************************************************
       ;; summary:  Add a new field to an export layout. 
       ;;
       ;; Args:     layoutName     name of export layout
       ;;           layoutField    field name to add
       ;;           layoutData     field data to save
       ;;
       ;; Return:   True
       ;;               
       ;; *******************************************************************
       vars:(
             thisField               ;; Field index variable
            ) 
       ;;
       ;; Initialize the directory
       (initLayoutDirectory)
       ;;
       ;; Retrieve the export layout from the Dictionary
       (if (= (setq currentLayout currentDirectory[layoutName]) #void)
          (error "unknownLayout"))
       ;; Test for existence of specified field
       (if (= (setq thisField (member layoutField currentLayout.fieldList)) false)
          (error "unknownField")
       )
       ;;
       ;; Test for existence of field metadata dictionary
       (if (= currentLayout.fieldData #void)
          (begin
             ;; Initialize a new field metadata dictionary
             (setq currentLayout.fieldData (^new Dictionary:))
             (loop for thisField from 0 until (length currentLayout.fieldList) do
                (setq currentLayout.fieldData[currentLayout.fieldList[thisField]] "0")
             )
          )
       )
       ;;
       ;; Assign data 
       (setq currentLayout.fieldData[layoutField] layoutData)
       ;;
       ;; Save export layout to dictionary
       (setq currentDirectory[layoutName] currentLayout)
       (saveLayoutDirectory)
       true) ;; end of updateField
    (defun updateLayout(layoutName layoutFile layoutType layoutTypeData layoutFields layoutData)
       ;; *******************************************************************
       ;; summary:  Update the indicated export layout.
       ;;
       ;;
       ;; Args:     layoutName      name of export layout
       ;;           layoutFile      export file
       ;;           layoutType      export as "delimited","fixed",etc. 
       ;;           layoutTypeData  custom delimiter
       ;;           layoutFields    tabbed list of field names to export
       ;;           layoutData      tabbed list of export-dependent metadata (length of field)
       ;;
       ;; Return:   True
       ;;               
       ;; *******************************************************************
       vars:(
             exportFields            ;; Field list vector
             exportData              ;; Field metadata
             exportFieldData         ;; Field metadata dictionary
             thisField               ;; Field index
            ) 

       ;;
       ;; Assure directory object is initialized
       (initLayoutDirectory)
       ;;
       ;; Retrieve specified layout
       (if (= (setq currentLayout currentDirectory[layoutName]) #void)
          (error "unknownFormat"))
       ;;
       ;; Convert the tabbed lists to a vectors
       (setq exportFields (stringToVector layoutFields "#\tab"))
       (if (<> layoutData #void) 
         (begin
            (setq exportData (stringToVector layoutData "#\tab"))
            ;;  
            ;; Create new field metadata dictionary
            (setq exportFieldData (^new Dictionary:))
            ;; Copy data to new dictionary
            (loop for thisField from 0 until (length exportFields) do
               (setq exportFieldData[exportFields[thisField]] exportData[thisField])
            )
         )
       )
       ;; Set layout fields
       (setq currentLayout.file layoutFile)
       (setq currentLayout.type layoutType)
       (setq currentLayout.typeData layoutTypeData)
       (setq currentLayout.fieldList exportFields)
       (setq currentLayout.fieldData exportFieldData)
       ;;
       ;; Save export layout to directory
       (setq currentDirectory[layoutName] currentLayout)
       (saveLayoutDirectory)

       true) ;; end of updateLayout
    ;; Initialize the export layout Lambda
    Continue::
    (setq myParent (myself))
    true) ;; end of exportLayoutLambda












;;**EXPORTKEY**:gausianEliminate
(defun gausianEliminate(w fudge)
;; *******************************************************************
;; name:     gausianEliminate
;; 
;; summary:  Triangulates the M by M+1 coefficient matrix representing 
;;           a system of M simultaneous linear equations in M variables.
;; Parms:    w:       The M by M+1 coefficient matrix.
;;           fudge:   (true) if we are to fudge around singular conditions.
;; Return:   w:       The M by M+1 matrix after triangulation.
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    vars:(i j k m m+1 maxRow tmp)
    (setq m+1 (length w[0]))
    (setq m (length w))
    (if (= m+1 (1+ m))
        (loop for i from 0 until m do
            (setq maxRow i)
            (loop for j from (1+ i) until m do
                (if (> (abs w[j][i]) (abs w[maxRow][i]))
                    (setq maxRow j)))
            (setq tmp w[i])
            (setq w[i] w[maxRow])
            (setq w[maxRow] tmp)
            (loop for j from (1+ i) until m do
                (loop for k from m to i by -1 do
                    (if (and fudge (isZero w[i][i]))
                        (if (= w[j][i] 0)
                            (setq w[j][k] (- w[j][k] (* w[i][k] 0)))
                            (setq w[j][k] (- w[j][k] (* w[i][k] (/ (1+ w[j][i]) (1+ w[i][i]))))))
                        (setq w[j][k] (- w[j][k] (* w[i][k] (/ w[j][i] w[i][i])))))
                    (if (not (isNumber w[j][k]))
                        (begin
                           (ringBell)
                           (writeln "gausianEliminate: w[j][k]=" w[j][k] 
                                    ",w[i][k]=" w[i][k] 
                                    ",w[j][i]=" w[j][i] 
                                    ",w[i][i]=" w[i][i] 
                                    ",i=" i ",j=" j ",k=" k))))))
        (error "!gausianEliminate: wrong size matrix!"))
    w)





























;;**EXPORTKEY**:gausianSubstitute
(defun gausianSubstitute(w fudge)
;; *******************************************************************
;; name:     gausianSubstitute
;; 
;; summary:  Returns the M coefficient vector from a triangulated matrix
;;           representing a system of M simultaneous linear equations in 
;;           M variables.
;; Parms:    w:       The M by M+1 triangulated matrix.
;;           fudge:   (true) if we are to fudge around singular conditions.
;; Return:   x:       The M coefficient vector.
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    vars:(j k m m+1 sum x)
    (setq m+1 (length w[0]))
    (setq m (length w))
    (setq x (new Vector: m 0))
    (if (= m+1 (1+ m))
        (loop for j from (-1+ m) to 0 by -1 do
            (setq sum 0)
            (loop for k from (1+ j) until m do
                (setq sum (+ sum (* w[j][k] x[k])))
                (if (not (isNumber sum))
                    (begin
                       (ringBell)
                       (writeln "gausianSubstitute: sum=" sum 
                                ",w[j][k]=" w[j][k] 
                                ",x[k]=" x[k] 
                                ",j=" j ",k=" k))))
            (if (and fudge (isZero w[j][j]))
                (if (= w[j][m] 0)
                    (setq x[j] 0)
                    (setq x[j] (/ (1+ (- w[j][m] sum)) (1+ w[j][j]))))
                (setq x[j] (/ (- w[j][m] sum) w[j][j])))
            (if (not (isNumber x[j]))
                (begin
                   (ringBell)
                   (writeln "gausianSubstitute: sum=" sum 
                            ",w[j][k]=" w[j][k] 
                            ",x[k]=" x[k] 
                            ",j=" j ",k=" k))))
        (error "!gausianSubstitute: wrong size matrix!"))
    x)
























;;**EXPORTKEY**:getRowByKey
(defun getRowByKey(SS key)
;; *************************************************************************************
;; summary:  This procedure gets a SmartRow from the specified Smarttable
;;           using the values in the key attributes of the key object. The key object
;;           may be any object which can be indexed by the key column names of the
;;           specified Smarttable.
;; args:     SS:                 The Smarttable to be searched for the row.
;;           key:                The key object whose attributes are used to find the row.
;; return:   row:                The SmartRow whose key attributes match the key object,
;;                               #void if there is no SmartRow whose key attributes match 
;;                               the key object, or an error if there is more than one
;;                               SmartRow whose key attributes match the key object.
;; *************************************************************************************
   vars:(i j n colCount row argVector (argIndex 0) valueIndex value rowIndex colVector)
   ;; If there are no rows, we can't find ours.
   (if (= SS.rowCount 0) (return #void))
   ;; Create the empty vector in which we will place the RowIndices for key columns.
   (setq argVector (new Vector: 0))
   ;; Search the Smarttable columns looking for key columns.
   (setq colCount SS.colCount)
   (setq colVector SS.colVector)
   (loop for i from 0 until colCount do
      (if (= colVector[i].colFormat key:)
          (begin
             ;; Use the key column's name to get the index value from the key object.
             (setq value key[colVector[i].name])
             ;; Use the index value from the key object to key column's RowIndex.
             (setq valueIndex colVector[i].valueIndex)
             (setq n (find valueIndex value))
             ;; If we can't find the value, then there is no matching row.
             (if (= n false) (return #void))
             (setq argVector[argIndex] valueIndex[n rowIndex:])
             (++ argIndex))))
   ;; Find the intersection of all RowIndices.
   (setq row (rowIntersect argVector))
   (if (= (type row) RowIndex:) (error "notUnique"))
   row)  





























;;**EXPORTKEY**:historyLambda
(defun historyLambda()
;; *******************************************************************
;; summary:  Uses the Workbench IO functions to open a workbench
;;           window and display the specified knowledge object (name)
;;           from the _HistoryDB or _Baysian in the specified workbench 
;;           window.
;; Args:     none    
;; Return:   true   Otherwise false, if the user cancels.
;; *******************************************************************
    ;; Persistent Variables
    pvars:((blockSize 100)     ;; The current history save-load blocking factor
           (blockIncr .000244140625) ;; The current history time slice blocking increment (allows 409600 records)
           myIndex             ;; The index of saved history time slices
           rowVector           ;; The row vector of the history time slice during save
           (tableSaves #{colCount: #void colVector: #void rowCount: #void showLimit: #void sparseSW: #void}) 
           ;; Child Lambdas
           archiveLegacyData   ;; Archive Legacy data into the History database
           delete              ;; Delete the tableLambda from the specified History database time slice
           exportData          ;; Export a History database time slice to a tab delimited file
           importData          ;; Import a tab delimited file into a History database time slice
           len                 ;; Return the count of saved History database time slices
           load                ;; Load the tableLambda from the specified History database time slice
           getKeys             ;; Get the list of History database time slice keys
           ref1                ;; Load the tableLambda from the specified History database time slice
           ref2                ;; Reference tableLambda time slices by index
           refCount            ;; Return the count of saved History database time slices
           refIndex            ;; Return the index of saved History database time slices
           save                ;; Save the tableLambda into the specified History database time slice
           set1                ;; Save the tableLambda into the specified History database time slice
           ) ;; end of pvars
    ;; Temporary Variables
    vars:(name theDate s i n keys
          commands VS endIndex newIndex)
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> getKeys #void) (goto Continue:))
    ;; Define the archive legacy data internal function.
    (defun archiveLegacyData(theLegacyDB theDate maxArchiveLimit startRow rowIncr fixupSW)
        vars:(STable               
              colHeaders 
              recordCount
              recordSize
              ) ;; end of temporary variables
        (if (= theDate #void)
            (setq theDate (input "Enter the archive date" (string (date (integer (now)))))))
        (if (isBoolean theDate) (return false))
        (setq theDate (date theDate))
        ;(setq colHeaders (theLegacyDB.Pv.getColVector))
        ;(validateColHeaders colHeaders)
        (if (> maxArchiveLimit 409600) (setq maxArchiveLimit 409600))
        (if (< startRow 0) (setq startRow 0))
        (if (< rowIncr 1) (setq rowIncr 1))
        (setq STable (theLegacyDB.tableToHistoryLambda maxArchiveLimit startRow rowIncr))
        (setq STable.Pv.colVector (objectToStructure (copy _validFields) #(#void)))
        (setq STable.Pv.colCount (length STable.Pv.colVector))
        (setq STable.Pv.saveVector #void)
        (writeln "History time slice closure size = " (setq recordSize (sizeof STable)))
        ;; Perform record fixup on the imported history table.
        (writeln "Running record fix up for " theDate)
        (historyFixupTables STable theDate)
        ;; Save the specified history table.
        (setq historyLambda[theDate] STable)
        (setq Stable #void)
        ;; Perform archive fixup (if necessary).
        (if (and fixupSW (<> _archiveFixupAll #void)) (_archiveFixupAll theDate))
        true) ;; end of archiveLegacyData
    ;; Delete the tableLambda from the specified History database time slice
    (defun delete(theDate)
        vars:(rowIndex rowCount 
              startRow endRow
              PvSaves blkCount blkKey blkVector blkIndex)
        ;; Delete the time slice from the index and save the index
        (refIndex)        
        (if (= myIndex[theDate] #void) (return true))
        (setq myIndex[theDate] #void)
        (setq _HistoryDB["*timeslices*"] myIndex)
        ;; Delete the tableLambda (without data) from the History database
        (setq PvSaves _HistoryDB[theDate])
        (setq _HistoryDB[theDate] #void)
        ;; Save the tableLambda row data in the History database
        (setq blkCount (ceiling (/ PvSaves.rowCount blockSize)))
        (loop for blkIndex from 0 until blkCount do 
            (setq blkKey (date (+ theDate (* blockIncr blkIndex))))
            (setq _HistoryDB[blkKey] #void) 
            ) ;; end block loop
        true) ;; end of delete
    ;; Define the export tableLambda internal function.
    (defun exportData(name theDate)
        vars:(STable)
        (setq STable historyLambda[theDate])
        (STable.exportTab name)
        (setq Stable #void)   
        true) ;; end of exportData
    ;; Define the getKeys internal function.
    (defun getKeys()
        vars:(i v)
        (setq v (new Vector: (length historyLambda)))
              (loop for i from 0 until (length myIndex) do
                 (setq v[i] myIndex[i 0]))
        v) ;; end of getKeys
    ;; Return the count of saved History database time slices
    (defun len() (refCount))
    ;; Load the tableLambda from the specified History database time slice.
    (defun load(theDate)
        vars:(rowIndex rowCount 
              startRow endRow STable
              PvSaves blkCount blkKey blkVector blkIndex)
        ;; Get the index check for the specified time slice
        (refIndex)        
        (if (= myIndex[theDate] #void) (return #void))
        ;; Load the tableLambda (without data) from the History database
        (tableLambda)
        (setq STable (new tableLambda))
        (setq PvSaves _HistoryDB[theDate])
        (setq STable.Pv (objectToStructure STable.Pv PvSaves))
        ;; Load the tableLambda row data from the History database
        (setq blkCount (ceiling (/ STable.rowCount blockSize)))
        (setq rowVector (^new Vector: object: STable.rowCount))
        (loop for blkIndex from 0 until blkCount do 
            (setq blkKey (date (+ theDate (* blockIncr (add1 blkIndex)))))
            (setq startRow (muli blkIndex blockSize))
            (setq endRow (addi startRow blockSize))
            (if (> endRow (length rowVector))
                (setq endRow (length rowVector)))
            (setq blkVector _HistoryDB[blkKey]) 
            (loop for rowIndex from startRow until endRow do
                (setq rowVector[rowIndex] blkVector[(subi rowIndex startRow)])
                ) ;; end row loop
            ) ;; end block loop
        (STable.saveSwap rowVector)
        (setq rowVector #void)
        STable) ;; end of load
    ;; Define the import tableLambda internal function.
    (defun importData(name theDate)
        vars:(STable startTime endTime)
        (tableLambda)
        (setq STable (new tableLambda))
        (STable.setSparse true)
        ;; Import the specified history table.
        (setq startTime (getTickCount 0))
        (STable.importTab name)
        (setq endTime (getTickCount startTime))
        ;(writeln "Importing " theDate " took " endTime " sec.")
        ;; Perform record fixup on the imported history table.
        (historyFixupTables STable theDate)
        ;; Save the specified history table.
        (setq startTime (getTickCount 0))
        (setq historyLambda[theDate] STable)
        (setq endTime (getTickCount startTime))
        ;(writeln "Saving " theDate " took " endTime " sec.")
        (setq Stable #void)   
        true) ;; end of importData
    ;; Reference tableLambda history time slices by index.
    (defun ref1(dateOrMember)
        (if (isSymbol dateOrMember) (return (myself)[Pv:][dateOrMember])) ;; Allows Lambda polymorphism.
        (if (not (isDate dateOrMember)) (error "invalidKey"))
        (load dateOrMember)) ;; end of ref1
    ;; Load the tableLambda from the specified History database time slice.
    (defun ref2(index1 index2)
        (if (= myIndex #void) (refIndex))
        (if (= index1 position:) (return (member index2 myIndex)))
        (if (not (isNumber index1)) (error "invalidKey"))
        (if (or (< index2 0) (> index2 1)) (error "invalidKey"))
        (if (= index2 0) (return myIndex[index1 index2]))
        (load myIndex[index1 0])) ;; end of ref2
    ;; Return the count of saved History database time slices
    (defun refCount() (length (refIndex)))
    ;; Return the index of saved History database time slices
    (defun refIndex()
        (setq myIndex _HistoryDB["*timeslices*"])
        (if (= myIndex #void) (setq myIndex (^new Directory:))) 
        myIndex) ;; end of refIndex
    ;; Save the tableLambda into the specified History database time slice.
    ;; Note: If the STable is #void, this will delete a time slice.
    (defun save(theDate STable)
        vars:(rowIndex rowCount
              startRow endRow
              PvSaves blkCount blkKey blkVector blkIndex)
        ;; Always erase any previous tableLambda for this date.
        (delete theDate)
        (if (= STable #void) (return true))
        ;; Add the new time slice to the index and save the index
        (refIndex)        
        (setq myIndex[theDate] true)
        (setq _HistoryDB["*timeslices*"] myIndex)
        ;; Save the tableLambda Pv variables in the History database
        (setq PvSaves (objectToStructure (copy tableSaves) STable.Pv))
        (setq _HistoryDB[theDate] PvSaves)
        ;; Save the tableLambda row data in the History database
        (setq rowVector STable.Pv.rowVector)
        (setq blkCount (ceiling (/ STable.rowCount blockSize)))
        (loop for blkIndex from 0 until blkCount do 
            (setq blkKey (date (+ theDate (* blockIncr (add1 blkIndex)))))
            (setq blkVector (^new Vector: object: blockSize))
            (setq startRow (muli blkIndex blockSize))
            (setq endRow (addi startRow blockSize))
            (if (> endRow (length rowVector))
                (begin 
                   (setq endRow (length rowVector))
                   (resize blkVector (subi endRow startRow))
                   )) ;; end if
            (loop for rowIndex from startRow until endRow do
                (setq blkVector[(subi rowIndex startRow)] rowVector[rowIndex])
                ) ;; end row loop
            ;(writeln "Saving block " blkIndex " as " blkKey " with length of " (length blkVector))
            (setq _HistoryDB[blkKey] blkVector) 
            ) ;; end block loop
        (setq rowVector #void)
        true) ;; end of save
    ;; Save the tableLambda into the specified History database time slice.
    ;; Note: If the STable is #void, this will delete a time slice.
    (defun set1(theDate STable) (save theDate STable))
    ;; Run the History Lambda
    Continue::
    ;; Make sure the History is properly assigned.
    (if (<> (type _HistoryDB) ObjectRepository:)
        (error "HistoryLambda" "The _HistoryDB is not assigned properly."))
    ;; Let user select from a series of commands.
    (setq commands #("Browse" "Import" "Export" "Fixup"))
    (setq n (select  "historyLambda"  "Please select a command." commands 0))
    (if (isBoolean n) (return false))
    ;; Import a History Stock table into the repository.
    (if (= n 1) 
        (begin
           (setq name #void)
           (setq theDate (input "Please enter an import date for the repository." "#Mar,1,1986")) 
           (if (isBoolean theDate) (return false))
           (setq theDate (date theDate))
           (importData name theDate)
           (return true)))
    ;; Export a History Stock table into the repository.
          (if (= n 2) 
        (begin
           (setq name #void)
           (setq commands (getKeys))
           (setq n (select  "historyLambda"  "Please select a date to export." commands 0))
           (if (isBoolean n) (return false))
           (setq theDate commands[n]) 
           (exportData name theDate)
           (return true)))
    ;; Fixup a History Stock table in the repository.
    (if (= n 3) 
        (begin
           (setq theDate (input "Fixup this date and prior year forward." "#Mar,1,1986")) 
           (if (isBoolean theDate) (return false))
           (setq theDate (date theDate))
           (historyFixupTables theDate)
           (return true)))
    ;; Browse History Stock Smarttable from History repository.
    (setq keys (getKeys))
    (setq n (select  "historyLambda"  "Please select a date to browse." keys 0))
    (if (isBoolean n)
        (return false)
        (setq name keys[n]))
    (setq s historyLambda[(date name)])
    (if (= s #void)
        (setq s "no History data available"))
    ;; Open window and display object in it.
    (displayWorkbenchWindow (string (append "History data for: " name)) s)
    true)

































;;**EXPORTKEY**:historyFixupTables
(defun historyFixupTables(theTable theDate) 
;; *******************************************************************
;; summary:  Perform a file fix up strategy against the Smarttable  
;;           currently being archived.
;; Args:     theTable   The tableLambda to be archived.
;;           theDate    The date of the first quarter to clean up.
;; Return:   true       
;; *******************************************************************
   vars:(recIndex recCount i j k n)
   ;; Locate the position of the starting fixup quarter in the History
   ;; repository.
   ;; Note: If the specified quarter is not in the repository, then return.
   (setq recCount (length theTable))
   (if (not (isLambda _historyArchiveFixup)) (return false))
   ;; Load each record of the Smarttable and send it to fix up.
   (writeln "Running record fix up for " theDate)
   (loop for recIndex from 0 until recCount do
       ;; Fix up the Smarttable record.
       (_historyArchiveFixup theTable[recIndex] theDate)
       ) ;; end of loop
   true)



























;;**EXPORTKEY**:historyViewer
(defun historyViewer(thisGroup)
;; *******************************************************************
;; summary:  History database viewer Lambda which manages the client
;;           viewing of the history database.
;; Args:     thisGroup    The history database group to view.    
;; Return:   true
;; *******************************************************************
    ;; Persistent Variables
    pvars:(myTable             ;; History tableLambda currently viewed
           myParent            ;; The current history viewer parent Lambda
           theGroup            ;; The current history group (time slice) viewed
           ;; Child Lambdas
           delimitedCells      ;; Return block of field values
           doAddRow            ;; Add a row to the current tableLambda
           doClear             ;; Clear the current tableLambda for gc
           doCellText          ;; Set the specified tableLambda cell value
           doDeleteRow         ;; Delete the specified row from the current tableLambda
           doFind              ;; Return the tableLambda row with the specified value
           doSort              ;; Sort the tableLambda on the specified column
           findAll             ;; Reset the current tableLambda to view all
           findRow             ;; Return the first row with the specified value
           getColumnHeaders    ;; Return the tab delimited column headers
           getRowBlock         ;; Return a tab delimited string of row values for a specified column
           ref2                ;; Return the specified cell of the tableLambda
           refColCount         ;; Return the column count in the tableLambda
           refColKeyness       ;; Return the keyness of the specified column
           refColName          ;; Return the name of the specified column
           refColType          ;; Return the type of the specified column
           refRowCount         ;; Return the row count in the tableLambda
           runCriteria         ;; Run the ad hoc query on the tableLambda
           runMeasurement      ;; Run the measurement strategy on the tableLambda
           runSelection        ;; Run the selection strategy on the tableLambda
           set2                ;; Set the specified tableLambda cell value
           setColKeyness       ;; Set the keyness of the specified column
           setColName          ;; Set the name of the specified column
           setColType          ;; Set the type of the specified column
           ) ;; end of pvars
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> ref2 #void) (goto Continue:))
    (defun delimitedCells(startRow endRow startCol endCol) (myTable.delimitedCells startRow endRow startCol endCol))
    (defun doAddRow(count row) (error "NoAccess"))
    (defun doClear() (setq theGroup #void) (setq myTable #void))
    (defun doCellText(col row value) (error "NoAccess"))
    (defun doDeleteRow(count row) (error "NoAccess"))
    (defun doFind(colName startRow value) (myTable.doFind colName startRow value))
    (defun doSort(columnName sortOrder) (myTable.doSort columnName sortOrder))
    (defun findAll() (myTable.findAll))
    (defun findRow(col value) (myTable.findRow col value))
    (defun getColumnHeaders(startCol endCol) (myTable.getColumnHeaders startCol endCol)) 
    (defun getRowBlock(startRow endRow colName) (myTable.getRowBlock startRow endRow colName)) 
    (defun refColCount() myTable.colCount)
    (defun refColKeyness(col) "n")
    (defun refColName(col) myTable.colVector[col 0])
    (defun refColType(col) "G")
    (defun ref2(col row) (myTable.refExportRow row)[col])
    (defun refRowCount() myTable.rowCount)
    (defun runCriteria(queryText) 
        vars:(algorithm)
        ;; Get the specified selection criteria specification.
        (selectCriteriaLambda)
        (setq algorithm (screenBuilder queryText))
        (algorithm myTable)
        true) ;; end runCriteria
    (defun runMeasurement(measureName) (measureCriteriaLambda.runCriteriaLambda measureName myTable))
    (defun runSelection(selectName) 
        vars:(selectSpec algorithm)
        ;; Get the specified selection criteria specification.
        (selectCriteriaLambda)
        (setq selectSpec (selectCriteriaLambda.Pv.getCriteria selectName))
        (setq algorithm (screenBuilder selectSpec))
        (algorithm myTable)
        true) ;; end runSelection
    (defun set2(col row newValue) (error "NoAccess"))
    (defun setColKeyness(col keyness) true)
    (defun setColName(col name) (myTable.addColumn name))
    (defun setColType(col type) true)
    ;; Initialize the Lambda
    Continue::
    (tableLambda)
    ;; Load the requested History time slice (only if necessary).
    (if (<> theGroup (date thisGroup))
        (begin
           (setq myTable #void)
           (setq theGroup (date thisGroup))
           (setq myTable historyLambda[theGroup])))
    (setq myParent (myself))
    (if (= myTable #void)
        (setq myTable (new tableLambda)))
    myTable) ;; end of historyViewer






















;;**EXPORTKEY**:honeyCombRepository
(defun honeyCombRepository(extentName compressSW) 
;; *******************************************************************
;; Summary:  Manage a honeycomb repository.
;; Parms:    extentName:    The name of the honeycomb extent file.
;;           compressSW:    The record compression switch (true/false).
;; Return:   true
;; *******************************************************************
   pvars:(;; Persistant variables
          (blockSize 500)                 ;; The number of records in each block
          fileID                          ;; The file ID of the repository extent
          fileName                        ;; The file name of the repository extent
          recordCompression               ;; The record compression switch (true/false)
          (recordSize 1000)               ;; The maximum size of each record
          ;; Method names
          updateBegin                     ;; Open the repository for update and access.
          updateEnd                       ;; Close the repository for update and access.
          updateRead                      ;; Read a single record from the repository.
          updateWrite                     ;; Write a single record to the repository.
          ) ;; end of persistant variables
   vars:(record)
   (defun updateBegin() 
   ;; *******************************************************************
   ;; Summary:  Open the repository for update and access.
   ;; Parms:    none
   ;; Return:   true
   ;; *******************************************************************
      vars:(record closeSW)
      (onError (lambda(errMsg) (setq fileID (fileOpen fileName 1 4)) true))
      (if (<> fileID #void) (updateEnd))
      (setq fileID (fileOpen fileName 0 4))
      true) ;; end updateBegin
   (defun updateEnd() 
   ;; *******************************************************************
   ;; Summary:  Close the repository for update and access.
   ;; Parms:    none
   ;; Return:   true
   ;; *******************************************************************
      vars:(record closeSW)
      (if (<> fileID #void) (fileClose fileID 1))
      (setq fileID #void)
      true) ;; end updateClose
   (defun updateRead(recIndex) 
   ;; *******************************************************************
   ;; Summary:  Read a single record from the repository.
   ;; Parms:    recIndex:  The record index of the record to be read.
   ;; Return:   The object record contents.
   ;; *******************************************************************
      vars:(record closeSW)
      (fileSeek fileID (muli recIndex blockSize) 1)
      (setq record (loadObject fileID))
      record) ;; end updateRead
   (defun updateWrite(recIndex record) 
   ;; *******************************************************************
   ;; Summary:  Write a single record to the repository.
   ;; Parms:    recIndex:  The record index of the record to be read.
   ;;           record:    The record object to be written.
   ;; Return:   true
   ;; *******************************************************************
      vars:((blockSize 4000))
      (fileSeek fileID (muli recIndex blockSize) 1)
      (saveObject fileID record)
      true) ;; end updateWrite
   ;; Initialize the honeycomb repository.
   (setq fileName extentName)
   (setq recordCompression compressSW)
   (setq fileID #void)
   (fileSeek fileID (muli recIndex blockSize) 1)
   (setq record (loadObject fileID))
   record) ;; end honeyCombRepository
   





























;;**EXPORTKEY**:honeyReadRecord
(defun honeyReadRecord(fileID recIndex) 
;; *******************************************************************
;; summary:  Perform a read of a record in the specified honeycomb file.
;; Parms:    This procedure accepts two arguments.
;;           fileID:    The fileID of the honeycomb file to read.
;;           recIndex:  The record index of the record to be read.
;; Return:   The object record contents.
;; *******************************************************************
   vars:(record (blockSize 4000))
   (fileSeek fileID (muli recIndex blockSize) 1)
   (setq record (loadObject fileID))
   record) ;; end honeyReadRecord
   





























;;**EXPORTKEY**:honeyWriteRecord
(defun honeyWriteRecord(fileID recIndex record) 
;; *******************************************************************
;; summary:  Perform a write of a record in the specified honeycomb file.
;; Parms:    This procedure accepts three arguments.
;;           fileID:    The fileID of the honeycomb file to read.
;;           recIndex:  The record index of the record to be read.
;;           record:    The record object to be written.
;; Return:   The object record contents.
;; *******************************************************************
   vars:((blockSize 4000))
   (fileSeek fileID (muli recIndex blockSize) 1)
   (saveObject fileID record)
   true) ;; end honeyWriteRecord
   





























;;**EXPORTKEY**:legacyBuilder
(defun legacyBuilder(aSpec)
;; *******************************************************************
;;  Summary: Builds a Legacy Screening function from the text specification   
;;           passed as an argument. The text specification is designed
;;           to allow ease of specification, recognition of similar 
;;           history screening strategies, and generation of history
;;           database screening strategies by genetic algorithms.
;; Note:     legacyBuilder refers to building history screening procedures,
;;           not to building graphic user interface screens.
;; Args:     aSpec      Screen specification text.
;; Return:   proc       a Legacy Screen Lambda, or false (if user cancels).   
;; *******************************************************************
   vars:(proc)
   (setq proc (_LegacyDB.queryBuilder aSpec))
   proc) ;; end legacyBuilder























;;**EXPORTKEY**:legacySetUserIndex
(defun legacySetUserIndex(newUserIndex)
;; *******************************************************************
;; summary:  Attach the current legacy librarian to the _LegacyDB database.
;; Args:     none    
;; Return:   true   
;; *******************************************************************
    ;; Switch the legacy user index to the new user.
    (abortTransaction _LegacyDB)
    (setq _LegacyUserIndex newUserIndex)
    (setq _LegacyPathName _UserLegacyPathName[_LegacyUserIndex])
    (setq _LegacyDB (new ObjectRepository: _LegacyPathName))
    ;; Attach the legacy librarian (if necessary).
    (if (not (isLambda _LegacyDB))
        (legacyAttachLibrarian))
    (_LegacyDB.switchUser)
    true) ;; end legacySetUserIndex
























;;**EXPORTKEY**:lexDebug
(defun lexDebug(theText) 
;; *******************************************************************
;; summary:  This procedure lexes the specified
;;           source with debug options on.
;; args:     source       => The source to be lexed.   
;; *******************************************************************
   vars:(proc lexresult)
   (debug true)
   (setq lexresult (_parser theText (setq proc (lambda() true)))) 
   (debug false)
   (ref proc Sc:))






























;;**EXPORTKEY**:lexDebugTree
(defun lexDebugTree(theText) 
;; *******************************************************************
;; summary:  This procedure lexes the specified
;;           source with debug options on.
;; args:     source       => The source to be lexed.   
;; *******************************************************************
   vars:(proc lexresult)
   (debug true)
   (setq lexresult (_parser theText (setq proc (lambda() true)))) 
   (debug false)
   lexresult)






























;;**EXPORTKEY**:loadObjectFile
(defun loadObjectFile(name type) 
;; *******************************************************************
;; summary:  Perform a single load of the specified object file.
;; Parms:    This procedure accepts two arguments
;;           name:      The name of the file to open.
;;           type:      The type of the file to open.
;;                      1 = a Spreadsheet object file.
;;                      2 = a Smarttable object file.
;;                      3 = a Workspace object file.
;;                      4 = a binary object file.
;; *******************************************************************
   vars:(fileID self)
   (setq fileID (fileOpen name 0 type))
   (setq self (loadObject fileID))
   (fileClose fileID 1)
   self)



























;;**EXPORTKEY**:makeGausianMatrix
(defun makeGausianMatrix(w)
;; *******************************************************************
;; name:     makeGausianMatrix
;; 
;; summary:  Returns the M by M+1 system of linear equations representing
;;           the coefficient derivative equations for the least squares
;;           error fit.
;; Parms:    w:       The N by M+1 matrix representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;; Return:   x:       The M by M+1 matrix representing the coefficients derivatives
;; Note:     See Sedgewick[2] chap 38.
;; *******************************************************************
    vars:(i j k m m+1 n x sum)
    (setq x (new Vector: (-1+ (length w[0]))))
    (setq n (length w))
    (setq m+1 (length w[0]))
    (setq m (-1+ m+1))
    (loop for i from 0 until m do
       (setq x[i] (new Vector: m+1))
       (loop for j from 0 until m+1 do
           (begin
              (setq sum 0)
              (loop for k from 0 until n do
                  (+= sum (* w[k][i] w[k][j]))
                  (if (not (isNumber sum))
                      (begin
                         (ringBell)
                         (writeln "makeGausianMatrix: w[k][i]=" w[k][i] ",w[k][j]=" w[k][j] ",i=" i ",j=" j ",k=" k))))
              (setq x[i][j] sum))))
    x)





























;;**EXPORTKEY**:makeGenebase
(defun  makeGenebase(name 
                     maxGenerations 
                     populationSize 
                     survival 
                     crossOver
                     mutation 
                     birthProc 
                     crossOverProc 
                     mutationProc 
                     fitnessProc 
                     genomeToKeyProc 
                     adults)
;; *************************************************************************************
;; name:     makeGenebase
;; 
;; summary:  Returns a new Genebase object from the specified arguments.
;; Args:     name:                The name of the new Genebase object. If name is #void,
;;                                the Genebase will be given a default name.
;;           maxGenerations:      The maximum number of generations.
;;           populationSize:      The count of genomes in each generation.
;;           survival:            The survival probability.
;;           crossOver:           The genetic cross over probability.
;;           mutation:            The genetic mutation probability.
;;           birthProc:           The birth procedure which must manage the initial 
;;                                random creation of an adult genome population from
;;                                scratch. (this procedure accepts one argument which is
;;                                the Genebase itself and returns the Genebase with its
;;                                adult population initialized).
;;           crossOverProc:       The genetic cross over procedure which maps two
;;                                genomes into a random "child" genome.
;;           mutationProc:        The genetic mutation procedure which maps a single
;;                                genome into a random "mutant" genome.
;;           fitnessProc:         The fitness procedure which maps each genome into
;;                                an integer fitness measure (higher is better).
;;           genomeToKeyProc:     The genome to key procedure which maps each genome
;;                                into a unique identifing symbol (its key).
;;           adults:              The Dictionary of genomes in the starting generation
;;                                or #void. If it is present, the Dictionary consists 
;;                                of unique genome keys and spairs as values. Each
;;                                spair contains two elements as follows:
;;                                  [0] Holds an individual genome object.
;;                                  [1] The integer fitness of the genome object or #void.
;;                                Note: The length must be less than or equal to 
;;                                      populationSize.
;; Return:   genebase:   The new Genebase object.
;; *************************************************************************************
    vars:(new-name)
    pvars:(genebase-counter)
    ;; We must create the new Genebase's name
    (if (= name #void)
        (setq new-name (append "Genebase" (++ genebase-counter)))
        (setq new-name (string name)))
    ;; The new Genebase's starting population must be a Dictionary.
    (if (not (isDictionary adult))
        (setq adult (makeDictionary)))
    ;; We must create the new Genebase using the builtin SmartLisp procedure
    (new Genebase: new-name
                   generation: 0 
                   maxGenerations: maxGenerations
                   populationSize: populationSize
                   survival: survival
                   crossOver: crossOver
                   mutation: mutation
                   birthProc: birthProc
                   crossOverProc: crossOverProc
                   mutationProc: mutationProc
                   fitnessProc: fitnessProc
                   genomeToKeyProc: genomeToKeyProc
                   adults: adults
                   children: (makeDictionary)
                   geneology: (makeDictionary)
                   history: (makeVector 0))) ;; end of makeGenebase




















;;**EXPORTKEY**:matrixFillColumn
(defun matrixFillColumn(w i x)
;; *******************************************************************
;; name:     matrixFillColumn
;; 
;; summary:  Fills the ith column vector with the input value.
;; Parms:    w:       An M by N matrix.
;;           i:       The ith column vector to fill.
;;           x:       The value to fill.
;; Return:   w:       The filled input matrix.
;; *******************************************************************
    vars:(j m n v)
    (setq m (length w))
    (if (isNumber x)
        (loop for j from 0 until m do
            (setq w[j][i] x))
        (loop for j from 0 until m do
            (setq w[j][i] x[j])))
    w)





























;;**EXPORTKEY**:matrixToSpreadsheet
(defun matrixToSpreadsheet(mat)
;; ********************************************************************
;; name:     matrixToSpreadsheet
;; 
;; summary:  Convert the specified M by N matrix into a Spreadsheet
;;           Lambda for easy viewing.
;; Parms:    mat:     The M by N matrix to be converted.
;;           ss:      The Spreadsheet version of the M by N matrix.
;; ********************************************************************
    vars:(ss i j m n)
    (setq m (length mat))
    (setq n (length mat[0]))
    (setq ss (new Spreadsheet:))
    (loop for i from 0 until m do
        (loop for j from 0 until n do
            (setq ss[j i] mat[i][j])))
    ss)





























;;**EXPORTKEY**:matrixToVector
(defun matrixToVector(w i)
;; *******************************************************************
;; summary:  Returns the vector extracted from the ith column of the
;;           input matrix: #(w[0][i] w[1][i] .... w[M][i])
;; Parms:    w:       An M by N matrix.
;;           i:       The ith column from which to extract the vector.
;; Return:   v:       The ith column vector of length M.
;; *******************************************************************
    vars:(j m n v)
    (setq m (length w))
    (setq v (new Vector: m))
    (loop for j from 0 until m do
            (setq v[j] w[j][i]))
    v)





























;;**EXPORTKEY**:meanProfit
(defun meanProfit(vr vx vy)
;; *******************************************************************
;; summary:  Returns the extended linear regression coefficient vector
;;           resulting from a linear regression on two vectors of equal
;;           length:  #(a  b  e  p)
;;           where a b e are as output by regress, and p represents the
;;           average y of the ten best preditions: a + bx.
;; Parms:    vr:      The linear regression coefficient vector.
;;           vx:      The factor vector.
;;           vy:      The value vector.
;; Return:   v:       The extended linear regression coefficient vector.
;;                    in the form of:    a1 b1 err1 y1 ëy1
;;                    where:             ä sqr(a + bx - y) = err
;;                    and:               y is the average of the y values
;;                                       ä (a + bx - y) / 5 = y
;;                                       for the top 5 (a + bx) predictions.
;;                    and:               ëy is the average of the y values
;;                                       ä (a + bx - y) / 5 = ëy
;;                                       for the worst 5 (a + bx) predictions.
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    vars:(w vs i j m n sortBest sortWorst ymean (profit 0) (loss 0))
    ;; Define the sort procedures
    (setq sortBest  (lambda(x y) (> x[2] y[2])))
    (setq sortWorst (lambda(x y) (< x[2] y[2])))
    ;; Find the input vector lengths.
    (setq n (length vx))
    (setq m (length vy))
    (setq n (min m n))
    (setq ymean (avg vy))
    ;; Make a matrix of factor, profit, and prediction. 
    (setq w (new Vector: n))
    (loop for i from 0 until n do
        (setq w[i] (new Vector: 3 vx[i] vy[i] (+ vr[0] (* vr[1] vx[i])))))
    ;; Average the delta profit from the best 5 predictions.
    (if (= vr[1] 0)
        (setq profit 0)
        (begin
           (sort w sortBest)
           (loop for i from 0 until 4 do
               (+= profit w[i][1]))
           (/= profit 4)
           (-= profit ymean)))
    ;; Average the delta loss from the worst 5 predictions.
    (if (= vr[1] 0)
        (setq loss 0)
        (begin
           (sort w sortWorst)
           (loop for i from 0 until 4 do
               (+= loss w[i][1]))
           (/= loss 4)
           (-= loss ymean)))
    ;; Make an output vector which includes the average profit and loss.
    (setq vs (new Vector: 5))
    (setq vs[0] vr[0])
    (setq vs[1] vr[1])
    (setq vs[2] vr[2])
    (setq vs[3] profit)
    (setq vs[4] loss)
    vs)





























;;**EXPORTKEY**:measureAddStrategy
(defun measureAddStrategy(measureTable theSpec theName theNotes)
;; *******************************************************************
;; Summary:  Add the specified measurement strategy to the History 
;;           Measurement Strategies table of measurement formulas to
;;           be used in backtesting. Each measurement strategy has a
;;           unique specification in the language defined by the
;;           measureBuilder Lambda.
;; Note:     We use the blackboard to store the results of backtesting
;;           for many different history database screening strategies.
;; Args:     measureTable   The History Measurement Strategy Table.
;;           theSpec        Specification of measurement strategy to add.
;;           theName        The name of strategy to add.
;; Return:   newName        The new name of the measurement strategy added
;;                          to the History Measurement Strategy Table. 
;; *******************************************************************
   vars:(i j m n rowCount
         rowIndex colIndex newName nameIndex 
         screenTable newScreenTable excludTable 
         ) ;; end of temporary variables
   (defun findRow(table colName keyValue)
   ;; Return the table row containing the specified value,
   ;; if no such row is found, then return false.
      vars:(rowIndex rowCount nameString)
      (setq rowCount table.rowCount)
      (loop for rowIndex from 0 until rowCount do
         (if (= keyValue table[colName rowIndex])
             (return rowIndex))
         ) ;; end loop
      false) ;; end findRow
   ;; Load the measureTable if we were not passed one.
   (if (= measureTable #void)
       (setq measureTable (measureLoadStrategies))
       ) ;; end if
   ;; This is a no op if the specification is void or empty.
   (if (= theSpec #void) (goto FixupMeasureTables:))
   ;; See if the measurement criteria table is already full.
   (if (>= measureTable.rowCount 50) (error "databaseFull"))
   ;; Determine if the specification is already in the table.
   ;; If it is present, then return the name assigned to it.
   (setq rowIndex (findRow measureTable Specification: theSpec))      
   (if (isNumber rowIndex)
       (return measureTable[|Name|: rowIndex]))
   ;; See if the user wishes to name the new measurement.
   (if (= theName #void)
       (setq newName (input "Please give your new measurement a name" ""))
       (setq newName theName))
   (setq nameIndex (findRow measureTable Name: newName))
   (if (or (isNumber nameIndex)
           (isNumber (member newName #("Specification" "Name" "Notes" "Last Date" "Count" "Size"))))
       (setq newName (append "Measure_" (now)))
       ) ;; end if
   ;; Add the measurement strategy and fixup the screenTotals 
   ;; table and all the detail results table (if any).
   (setq rowIndex measureTable.rowCount)
   (addRow measureTable 1 rowIndex)
   (setq measureTable[Specification: rowIndex] theSpec)
   (setq measureTable[Notes: rowIndex] theNotes)
   (setq measureTable[Lambda: rowIndex] (measureBuilder theSpec))
   (setq measureTable[Name: rowIndex] newName)
   (setq measureTable[Unfair: rowIndex] measureBuilder.unfairFields)
   ;; Erase all items from the Blackboard except the screenLambda,
   ;; excludLambda, and measureLambda strategy tables.
   FixupMeasureTables::
   (setq screenTable _Blackboard.screenLambda)
   (setq excludTable _Blackboard.excludLambda)
   (clear _Blackboard)
   ;; Create an all new Blackboard screenLambda strategy table
   ;; with column names which match the measurement strategy names.
   (setq newScreenTable (screenLoadStrategies))
   (setq rowCount measureTable.rowCount)
   (loop for rowIndex from 0 until rowCount do
       (setq measureTable[Lambda: rowIndex] (measureBuilder measureTable[Specification: rowIndex]))
       (setq colIndex newScreenTable.colCount)
       (addCol newScreenTable 1 colIndex)
       (setq newScreenTable.colVector[colIndex].name (symbol measureTable[Name: rowIndex]))
       ) ;; end loop
   ;; Add old selection strategies to the new screenLambda strategy table.
   (if (<> screenTable #void)
       (setq rowCount screenTable.rowCount)
       (setq rowCount 0))
   (loop for rowIndex from 0 until rowCount do
       (setq newScreenTable[Specification: rowIndex] screenTable[Specification: rowIndex])
       (setq newScreenTable[Name: rowIndex] screenTable[Name: rowIndex])
       (setq newScreenTable[Notes: rowIndex] screenTable[Notes: rowIndex])
       ) ;; end loop 
   ;; Save the new Lambda strategy tables in the blackboard. 
   (beginTransaction _Blackboard)
   (setq _Blackboard.screenLambda newScreenTable)
   (setq _Blackboard.excludLambda excludTable)
   (setq _Blackboard.measureLambda measureTable)
   (commitTransaction _Blackboard)
   newName) ;; end measureAddStrategy





















;;**EXPORTKEY**:measureBuilder
(defun measureBuilder(aSpec)
;; *******************************************************************
;; Summary:  Builds a History Measurement function from the text specification   
;;           passed as an argument. The text specification is designed
;;           to allow ease of specification, recognition of similar 
;;           history measurement strategies, and generation of history
;;           database measurement strategies by genetic algorithms.
;; Args:     aSpec      Measurement specification text.
;; Return:   proc       aHistory Measurement Lambda.   
;; *******************************************************************
   pvars:((debugOn false)         ;; Display generated Lambda on switch
          filterFields            ;; Dictionary of valid field names
          unfairFields            ;; Dictionary of unfair field names
          fixupLambda              ;; Lambda for expression transformation
          syntaxLambda             ;; Lambda for expression syntax validation
          rightCommand            ;; The right most statistic command
          ;; Methods list 
          assertRules             ;; Method for defining fix up rules
          buildBody               ;; Method to build the SmartLisp expression body
          buildName               ;; Method for recognizing a History field name
          cmdSet                  ;; Method for parsing the "set" command
          cmdMeasure              ;; Method for parsing the "avg min max etc" commands
          errorStop               ;; Method to manage error conditions
          pairToVector            ;; Method for converting a List into a Vector of terms
          ) ;; end of persistent variables
   vars:(proc aCmd i n cmds scmd theSpec cmdVector setCount filterProc)
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> errorStop #void) (goto Continue:))
   ;; Initialize the inline child Lambdas.
   (defun assertRules()
       ;; We only need to run this once.
       (if (isLambda fixupLambda) (return true))
       ;; Define the expression fix up rules.
       (rulesLib)
       (setq fixupLambda (new rulesLib))
       ;; Make sure there are no duplication errors
       (if (= fixupLambda.rulesDic rulesLib.rulesDic) (error "dupRules"))
       ;; Operator name, function name, and number recognition rules
       (fixupLambda.assert $OP:(lambda(x) 
                                 vars:(s (d #{and and or or min min max max + + - - * * / / ** expt < < <= <= = = <> <> >= >= > >})) 
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $REL:(lambda(x) vars:((d #{< < <= <= = = <> <> >= >= > >})) (if (isMember x d) d[x])))
       (fixupLambda.assert $FN1:(lambda(x) 
                                 vars:(s (d #{sin sin cos cos tan tan log log exp exp sqrt sqrt})) 
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $FN2:(lambda(x) 
                                 vars:(s (d #{min min max max}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $NUM:(lambda(x) (if (isNumber x) x)))
       ;; Infix to Prefix notation production rules
       (fixupLambda.assert $PFN:(lambda(a fn x y b) vars:(p) (setq p (list fn x y)) (if (<> b #void) (setq p (append (list p) b))) (if (<> a #void) (setq p (append a p))) p))
       (fixupLambda.assert '($a* $X <$FN=$OP> $Y $b*) '(<$PFN> $a $FN $X $Y $b))
       ;; Symbol referecing production rules
       (fixupLambda.assert $SYM:(lambda(x) (if (= (type x) Symbol:) (makeQuotedSymbol x))))
       (fixupLambda.assert $VAR:(lambda(x) (if (and (= (type x) Symbol:) (<> x[0] #\_)) x)))
       (fixupLambda.assert $STR:(lambda(x) (if (isString x) (makeQuotedSymbol x))))
       (fixupLambda.assert '(ref $X <$Y=$SYM>) '(ref $X $Y))
       (fixupLambda.assert '(ref $X <$Y=$STR>) '(ref $X $Y))
       ;; Constant folding production rules
       (fixupLambda.assert $DIV:(lambda(x) (error "measureBuild" (append "measureBuilder divide by zero on: " (string x true)))))
       (fixupLambda.assert $FOLD1:(lambda(op x) vars:(f) (setq f (getGlobalValue (symbol op))) (f x)))
       (fixupLambda.assert $FOLD2:(lambda(op x y) vars:(f) (setq f (getGlobalValue (symbol op))) (f x y)))
       (fixupLambda.assert '(<$Y=$OP> <$X=$NUM> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '(<$Y=$FN1> <$X=$NUM>) '(<$FOLD1> $Y $X))
       (fixupLambda.assert '(<$Y=$FN2> <$X=$NUM> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '(** $X $Y) '(expt $X $Y))
       ;; Algebraic expression reduction rules
       (fixupLambda.assert '(/ $X $X) 1)
       (fixupLambda.assert '(+ $X 0) '$X)
       (fixupLambda.assert '(- $X 0) '$X)
       (fixupLambda.assert '(* $X 0) 0)
       (fixupLambda.assert '(/ 0 0) 0)
       (fixupLambda.assert '(/ $X 0) '(<$DIV> (/ $X 0)))
       (fixupLambda.assert '(expt $X 0) 1)
       (fixupLambda.assert '(+ 0 $X) '$X)
       (fixupLambda.assert '(* 0 $X) 0)
       (fixupLambda.assert '(/ 0 $X) 0)
       (fixupLambda.assert '(expt 0 $X) 0)
       (fixupLambda.assert '(* $X 1) '$X)
       (fixupLambda.assert '(/ $X 1) '$X)
       (fixupLambda.assert '(expt $X 1) '$X)
       (fixupLambda.assert '(* 1 $X) '$X)
       (fixupLambda.assert '(expt 1 $X) 1)
       ;; One based indices reduction rules
       (fixupLambda.assert '(ref (ref x $X) <$Y=$NUM>) '(ref (ref x $X) (sub1 $Y)))
       ;; Excess parentheses reduction rules
       (fixupLambda.assert '(($X*)) '$X)
       (fixupLambda.assert '(<$X=$VAR>) '$X)
       ;; Define the expression syntax validation rules.
       (setq syntaxLambda (new rulesLib))
       (syntaxLambda.setFailure false)
       ;; Make sure there are no duplication errors
       (if (= syntaxLambda.rulesDic fixupLambda.rulesDic) (error "dupRules"))
       ;; Prefix notation recognition rules
       (syntaxLambda.assert '(ref $X $Z) false)
       (syntaxLambda.assert '(<$Y=$FN1> <$X=$NUM>) '(<$FOLD1> $Y $X))
       (syntaxLambda.assert '(<$Y=$OP> $X $Z) false)
       (syntaxLambda.assert '(<$Y=$FN1> $X) false)
       (syntaxLambda.assert '(<$Y=$FN2> $X $Z) false)
       ;; Unrecognized syntax error rules
       (syntaxLambda.assert $ERR:(lambda(x) (error "measureBuild" (append "measureBuilder syntax error on: " (string x true)))))
       (syntaxLambda.assert '$ALL$ '(<$ERR> $ALL$))
       ;; Operator and function name recognition lambda rules
       (syntaxLambda.assert $OP:(lambda(x) 
                                 vars:(s (d #{and and or or min min max max + + - - * * / / expt expt < < <= <= = = <> <> >= >= > >}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FN1:(lambda(x) 
                                 vars:(s (d #{sin sin sub1 sub1 cos cos tan tan log log exp exp sqrt sqrt}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FN2:(lambda(x)
                                 vars:(s (d #{min min max max mapc mapc}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FOLD1:(lambda(op x) vars:(f) (setq f (getGlobalValue (symbol op))) (f x)))
       (syntaxLambda.assert $NUM:(lambda(x) (if (isNumber x) x)))
       true) ;; end assertRules
   (defun buildBody(v p begIndex endIndex)
   ;; Define the build expression body function. This function builds SmartLisp
   ;;  expression bodies.
   ;;  such as (1 {A1} true etc).
       vars:(fieldName s i n)
       (setq s "")
       (loop for i from begIndex until endIndex do
           (setq s (append s " " (buildName v[i] p)))
           ) ;; end loop
       ;; Apply expression transformation rules.
       (fixupLambda.setVerbose false)
       (setq s (lisp s arithmetic:))
       (setq fixupLambda.singlePass false)
       (setq s (fixupLambda.apply s))
       (setq s (syntaxLambda.apply s))
       (setq s (string s true))
       s) ;; end buildBody
   (defun buildName(s p)
   ;; Define the build name function. This function recognizes History
   ;;  field names inclosed in vertical bar | symbols, or SmarLisp constants
   ;;  such as (1 {A1} true etc).
       vars:(fieldName)
       (cond ;; Recognize a History field name inclosed in vertical bar | symbols.
             ;;  Return the construct: p.|Fieldname|
             ((and (isType Symbol: s) (= s[0] #\|))
              (begin
                 (setq fieldName (makeSymbol (mid s 1 (- (length s) 2))))
                 (if (= (member fieldName _validFields) false)
                     (error "buildName"))
                 (setq filterFields[fieldName] _validFields[fieldName])
                 (setq unfairFields[fieldName] true)
                 (append p "." s)))
             ;; Recognize a Legacy field name not inclosed in vertical bar | symbols.
             ;;  Return the construct: p.|Fieldname|
             ((and (isType Symbol: s) (<> (member s _validFields) false))
              (begin
                 (setq fieldName s)
                 (if (= (member fieldName _validFields) false)
                     (error "buildName"))
                 (setq filterFields[fieldName] _validFields[fieldName])
                 (setq unfairFields[fieldName] true)
                 (append p "." fieldName)))
             ;; Recognize all other non-arithmetic operator symbols.
             ;;  Convert the symbol to a string contant.
             ;((and (isType Symbol: s) (= (member s #(+ - * / < > >= <= = <> ref |(| |)| |[| |]|)) false))
             ; (append "{" s "}"))
             ;; Recognize all quoted symbols.
             ;;  Convert the quoted symbol to a string contant.
             ((isType QuotedSymbol: s)
              (append "{" s "}"))
             ;; Assume we have a SmartLisp constant.
             ;;  Return the constant unaltered.
             (else s)
             ) ;; end cond
        ) ;; end buildName
   (defun cmdSet(parms s)
   ;; Define the command parser for the set.
   ;;   for example: "set,X,(,|3 Month Profit|,+,|6 Month Profit|,)"
       vars:(parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (< parmCnt 3)
           (error "measureBuild" s)) 
       (if (<> parms[0] "set")
           (error "measureBuilder" s)) 
       ;; Build the resulting assignment command.
       (setq outS (append "      (define " parms[1] " " (buildBody parms "_x" 2 parmCnt) ")" _eol "   "))
       outS) ;; end cmdSet
   (defun cmdMeasure(parms s)
   ;; Define the command parser for the (sum avg min max var std).
   ;;   for example: "avg,|3 Month Profit|"
       vars:(parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (< parmCnt 2)
           (error "measureBuild" s)) 
       (if (isBoolean (member parms[0] #("sum" "avg" "min" "max" "var" "std")))
           (error "measureBuild" s))
       (setq rightCommand parms[0])
       ;; Build the resulting statistic command.
       (cond
           ((isNumber (member rightCommand #(sum avg)))
            (setq outS (append "     (+= _measure " (buildBody parms "_x" 1 parmCnt) ")" _eol "   ")))
           ((isNumber (member rightCommand #(var std)))
            (begin
               (setq outS (append "      (setq _temp " (buildBody parms "_x" 1 parmCnt) ")" _eol "   "))
               (setq outS (append outS "      (+= _mean _temp)" _eol "   "))
               (setq outS (append outS "      (+= _measure (* _temp _temp))"  _eol "   "))))
           ((= rightCommand "min")
            (setq outS (append "      (setq _measure (min _measure " (buildBody parms "_x" 1 parmCnt) "))" _eol "   ")))
           ((= rightCommand "max")
            (setq outS (append "      (setq _measure (max _measure " (buildBody parms "_x" 1 parmCnt) "))" _eol "   ")))
           ) ;; end cond
       outS) ;; end cmdMeasure
   (defun errorStop(err) (writeln "measureBuilder: " err) false)
   (defun pairToVector(v p)
       vars:(i n result)
       (if (= v #void) (setq v (new Vector: 0)))
       (setq n (length p))
       (loop for i from 0 until n do
          (if (isAtom p[i])
              (setq v[(length v)] p[i])
              (begin
                 (setq v[(length v)] |(|:)
                 (setq v (pairToVector v p[i]))
                 (setq v[(length v)] |)|:)
                 )) ;; end if
          ) ;; end loop
       v) ;; end pairToVector
   ;; Run the Lambda.
   Continue::
   (onError errStop)
   (setq filterFields (new Dictionary:))
   (setq unfairFields (new Dictionary:))
   (assertRules)
   ;; Break the command up into its sub-commands.
   (setq cmds (stringToVector aSpec ";"))
   ;; Parse each set command and append to the specification.
   (setq theSpec "")
   (setq setCount (subi (length cmds) 1))
   ;; Parse each set command and append to the specification.
   (loop for i from 0 until setCount do
       (setq cmdVector (pairToVector #void (lisp cmds[i] arithmetic:)))
       (setq scmd (cmdSet cmdVector cmds[i]))
       (setq theSpec (append theSpec scmd))
       ) ;end loop
   ;; Parse the final statistics command and append to the specification.
   (setq cmdVector (pairToVector #void (lisp cmds[i] arithmetic:)))
   (setq scmd (cmdMeasure cmdVector cmds[i]))
   (setq theSpec (append theSpec scmd))
   ;; Convert the measurement specification into a procedure.
   (setq proc (append "(lambda(_ST)" _eol "   "))
   (setq proc (append proc "vars:(_x _rowIndex)" _eol "   "))
   (setq proc (append proc "(define _rowCount _ST.rowCount)" _eol "   "))
   (setq proc (append proc "(define _theCount 0)" _eol "   "))
   (setq proc (append proc "(define _mean 0)" _eol "   "))
   (setq proc (append proc "(define _temp 0)" _eol "   "))
   (cond 
       ((= rightCommand "max")
        (setq proc (append proc "(define _measure -99999999999999)" _eol "   ")))
       ((= rightCommand "min")
        (setq proc (append proc "(define _measure 99999999999999)" _eol "   ")))
       (else
        (setq proc (append proc "(define _measure 0)" _eol "   ")))
       ) ;; end cond
   (setq proc (append proc "(defun measure(_x)" _eol "   "))
   (setq proc (append proc "   (onError (lambda(err) (-- _theCount) 0))" _eol))
   (setq proc (append proc theSpec "   ) ;; end measure" _eol "   "))
   (setq filterProc "")
   (loop for i from 0 until (length filterFields) do
      (if (<> filterFields[i 1] #void)
          (begin
             (setq filterProc (append filterProc  
                                _eol 
                                "              (" 
                                filterFields[i 1] 
                                " _x.|" 
                                filterFields[i 0] 
                                "|)")) 
             ))) ;; end loop
   (setq proc (append proc "(loop for _rowIndex from 0 until _rowCount do" _eol "   "))
   (setq proc (append proc "    (setq _x _ST[_rowIndex])" _eol "   "))
   (setq proc (append proc "    (if (and "))
   (setq proc (append proc filterProc ")" _eol "   "))
   (setq proc (append proc "        (begin" _eol "   "))
   (setq proc (append proc "           (++ _theCount)" _eol "   "))
   (setq proc (append proc "           (measure _x)" _eol "   "))
   (setq proc (append proc "           )) ;; end if" _eol "   "))
   (setq proc (append proc "    ) ;; end loop" _eol "   "))
   (cond
       ((= rightCommand "avg")
        (setq proc (append proc "(if (> _theCount 0) (/= _measure _theCount))" _eol "   ")))
       ((= rightCommand "std")
        (begin
           (setq proc (append proc "(-= _measure  (* _mean _mean _theCount))" _eol "   "))
           (setq proc (append proc "(if (> _theCount 0) (/= _measure _theCount))" _eol "   "))))
       ((= rightCommand "var")
        (setq proc (append proc "(-= _measure  (* _mean _mean _theCount))" _eol "   ")))
       ) ;; end cond
   (setq proc (append proc "(if (= _theCount 0) (setq _measure #void))" _eol "   "))
   (setq proc (append proc "_measure) ;; end lambda"))
   (if (= debugOn true) (writeln proc)) ;; Use for testing only
   (setq proc (eval proc))
   proc) ;; end of measureBuilder


;;  ***NOTES***:
;; 
;; The selection measurement specification is composed of a series of commands
;; each separated from the other by the semi-colon ; symbol. Some examples
;; might be:
;;
;;     "min,|12 Month Profit|"
;;
;;     "avg,(,|6 Month Profit|,-,|3 Month Profit|,)"
;;
;;     "set,X,|3 Month Profit|;set,Y,(,|6 Month Profit|,-,X,);avg,Y"
;;
;; Notice the semi-colon ; symbol which separates the various commands. Also
;; notice the comma , symbols which separate the command arguments one from
;; another within a command. Each command is composed of a number of arguments.
;; The number of arguments may vary and is dependent upon the command verb
;; which is the first argument in any command, and upon the number and depth
;; of nested parentheses within the expression.
;;
;; Seven command verbs are currently supported. They are as follows:
;;
;;            set  sum  avg  min  max  var  std
;;
;; The final (right most) command must be one of (sum avg min max var std). All
;; of the left most commands must be set commands. Obviously, the set command
;; is only present to allow assignment of temporary variables. Any multiple
;; command measurement can be reexpressed as a single command measurement without
;; temporary variables at the expense of deeply nested parentheses.
;;
;;
;; Each command verb has a distinct meaning and requires somewhat different
;; command arguments. Remember that each command generates code which operates
;; on a stock table by reducing the number of stocks in the table. This is
;; called screening. Commands are evaluated left to right so that the righmost
;; commands always reduces the stock table which results from the leftmost
;; command.
;;
;;
;; **EXAMPLES**
;;
;; For instance we might want to measure the standard deviation of annual profit
;; for a group of selected stocks.
;;
;;     "std,|12 Month Profit|"
;;
;; We might want to measure the worst case quarterly loss for a group of 
;; selected stocks.
;;
;;     "min,|3 Month Profit|"
;;
;;




















;;**EXPORTKEY**:measureClearAll
(defun measureClearAll()
;; *******************************************************************
;; Summary:  Erase all items from the Blackboard except the screenLambda,
;;           excludLambda, and measureLambda strategy tables.
;; Note:     We use the blackboard to store the results of backtesting
;;           for many different history database screening strategies.
;; Args:     none
;; Return:   true
;; *******************************************************************
   vars:(i j m n rowCount measureTable
         rowIndex colIndex newName nameIndex 
         screenTable newScreenTable excludTable 
         ) ;; end of temporary variables
   ;; Load the measureTable.
   (setq measureTable (measureLoadStrategies))
   ;; Erase all items from the Blackboard except the screenLambda,
   ;; excludLambda, and measureLambda strategy tables.
   (setq screenTable _Blackboard.screenLambda)
   (setq excludTable _Blackboard.excludLambda)
   (clear _Blackboard)
   ;; Create an all new Blackboard screenLambda strategy table
   ;; with column names which match the measurement strategy names.
   (setq newScreenTable (screenLoadStrategies))
   (setq rowCount measureTable.rowCount)
   (loop for rowIndex from 0 until rowCount do
       (setq measureTable[Lambda: rowIndex] (measureBuilder measureTable[Specification: rowIndex]))
       (setq colIndex newScreenTable.colCount)
       (addCol newScreenTable 1 colIndex)
       (setq newScreenTable.colVector[colIndex].name (symbol measureTable[Name: rowIndex]))
       ) ;; end loop 
   ;; Add old selection strategies to the new screenLambda strategy table.
   (if (<> screenTable #void)
       (setq rowCount screenTable.rowCount)
       (setq rowCount 0))
   (loop for rowIndex from 0 until rowCount do
       (setq newScreenTable[Specification: rowIndex] screenTable[Specification: rowIndex])
       (setq newScreenTable[Name: rowIndex] screenTable[Name: rowIndex])
       (setq newScreenTable[Notes: rowIndex] screenTable[Notes: rowIndex])
       ) ;; end loop 
   ;; Save the new Lambda strategy tables in the blackboard. 
   (beginTransaction _Blackboard)
   (setq _Blackboard.screenLambda newScreenTable)
   (setq _Blackboard.excludLambda excludTable)
   (setq _Blackboard.measureLambda measureTable)
   (commitTransaction _Blackboard)
   true) ;; end measureClearAll





















;;**EXPORTKEY**:measureCriteriaLambda
(defun measureCriteriaLambda()
;; *******************************************************************
;;  Summary: This Lambda is in charge of History database measurement
;;           Screening Strategies which provide the model with
;;           measurement criteria for measuring the success of
;;           available selection criteria.
;; Args:     none
;; Return:   myTView     The tableview of the measurement criteria. 
;; *******************************************************************
    pvars:(measureST                  ;; The measurement criteria Smarttable
           myParent                   ;; The parent Lambda object reference
           myTView                    ;; The measurement criteria Tableview
           unfairFields               ;; The Dictionary of unfair fields for this measurement criteria
           ;; Child Lambdas
           addNewExcludeName          ;; Add a new field name to be excluded from automatic strategies. 
           addNewCriteria             ;; Add the specified new criteria to the blackboard.
           checkFieldName             ;; Validity check a single or compound unfair field name
           Checkin
           deleteCriteria             ;; Delete the specified criteria from the blackboard.
           deleteExcludeName          ;; Delete a field name to be excluded from automatic strategies. 
           doClear
           findCriteriaName           ;; Return the measurement criteria with the specified unique name. 
           findCriteriaSpecification  ;; Return the measurement criteria with the specified unique Specification. 
           getColumnHeaders           ;; Return the History database valid column headers.
           getCriteria                ;; Return the measurement criteria strategy specification string
           getCriteriaLambda           ;; Return the measurement criteria strategy Lambda
           loadCondition 
           loadCriteriaName           ;; Load list of all criteria names for display in the names list box. 
           loadCriteriaNotes          ;; Return the measurement criteria notes string
           loadExcludeNames           ;; Load list of all field names to be excluded from automatic strategies. 
           loadFieldName  
           loadLogic 
           loadSort  
           loadUnfairFields           ;; Load Dictionary of all unfair field names to be excluded from automatic strategies. 
           loadValue
           ref2
           refColCount
           refColKeyness
           refColName
           refColType
           refRowCount
           renameCriteria             ;; Rename the specified criteria. Make sure the
           runCriteriaLambda           ;; Run the measurement criteria strategy Lambda
           saveCriteriaNotes          ;; Save the specified criteria notes.
           set2
           ) ;; end of persistant variables
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> ref2 #void) (goto ReInitialize:))
    ;; Initialize the inline child Lambdas.
    (defun addNewExcludeName(criteriaName unfairField)
    ;; Add a new unfair field name to be excluded from automatic strategies.
       vars:(rowIndex fieldName)
       (setq fieldName (symbol unfairField))
       (if (not (checkFieldName fieldName)) (error "invalidName"))
       (beginTransaction _Blackboard) 
       (setq measureST (measureLoadStrategies))      
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "NoOldName")
           )) ;;end if
       ;; Add the unfair field name to the measurement criteria table.
       (setq measureST[rowIndex].Unfair[fieldName] true)
       (setq _Blackboard.measureLambda measureST)
       (commitTransaction _Blackboard)
       unfairField) ;; end addNewExcludeName
    (defun addNewCriteria(newSpec newName newNotes)
    ;; Add the specified new criteria to the blackboard.
       (setq newName (measureAddStrategy #void newSpec newName newNotes))
       (setq measureST #void)
       newName) ;; end addNewCriteria
    (defun checkFieldName(fieldName)
       vars:(vec ftype firstName middleName lastName)
       (setq vec (stringToVector fieldName "["))
       (setq firstName (symbol vec[0]))
       (if (> (length vec) 1)
           (begin
              (setq middleName (symbol (stringToVector vec[1] "].")[0]))
              (setq lastName (symbol (stringToVector vec[1] "].")[1]))
              )
           (begin
              (setq middleName #void)
              (setq lastName #void)
              )) ;; end if
       (if (isMember firstName _validFields)
           (setq ftype _validFields[firstName])
           (error "unknownField"))
       (if (= ftype |isDictionary|:)
           (begin 
           (if (and (= firstName "DepProducts") 
                    (or (isMember lastName _depRollupFields) (error "unknownField")))
               (setq ftype _depRollupFields[lastName]))
           (if (and (= firstName "DepAccounts") 
                    (or (isMember lastName _depDetailFields) (error "unknownField")))
               (setq ftype _depDetailFields[lastName]))
           (if (and (= firstName "CrdProducts") 
                    (or (isMember lastName _cardRollupFields) (error "unknownField")))
               (setq ftype _cardRollupFields[lastName]))
           (if (and (= firstName "CrdAccounts") 
                    (or (isMember lastName _cardDetailFields) (error "unknownField")))
               (setq ftype _cardDetailFields[lastName]))
           (if (and (= firstName "LonProducts") 
                    (or (isMember lastName _loanRollupFields) (error "unknownField")))
               (setq ftype _loanRollupFields[lastName]))
           (if (and (= firstName "LonAccounts") 
                    (or (isMember lastName _loanDetailFields) (error "unknownField")))
               (setq ftype _loanDetailFields[lastName]))
           (if (and (= firstName "LinProducts") 
                    (or (isMember lastName _lineRollupFields) (error "unknownField")))
               (setq ftype _lineRollupFields[lastName]))
           (if (and (= firstName "LinAccounts") 
                    (or (isMember lastName _lineDetailFields) (error "unknownField")))
               (setq ftype _lineDetailFields[lastName]))
           )) ;; end if
       (if (= ftype |isDictionary|:) (error "unknownField"))
       true) ;; end checkFieldName
    (defun Checkin(title) (error "accessDenied"))
    (defun deleteCriteria(criteriaName) 
    ;; Delete the specified criteria from the blackboard.
       vars:(rowIndex detailTableKey)
       (beginTransaction _Blackboard) 
       (setq measureST (measureLoadStrategies))      
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "NoOldName")
           )) ;;end if
       ;; Delete the measurement criteria and its detail results table (if any).
       (setq detailTableKey (append "measureLambda:" measureST[rowIndex].Specification))
       (setq _Blackboard[detailTableKey] #void)
       (deleteRow measureST 1 rowIndex)
       (setq _Blackboard.measureLambda measureST)
       (commitTransaction _Blackboard)
       (measureClearAll)
       true) ;; end saveCriteriaNotes
    (defun deleteExcludeName(criteriaName unfairField)
    ;; Delete an unfair field name to be excluded from automatic strategies.
       vars:(rowIndex fieldName)
       (setq fieldName (symbol unfairField))
       (beginTransaction _Blackboard) 
       (setq measureST (measureLoadStrategies))      
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "NoOldName")
           )) ;;end if
       ;; Delete the unfair field name to the measurement criteria table.
       (setq measureST[rowIndex].Unfair[fieldName] #void)
       (setq _Blackboard.measureLambda measureST)
       (commitTransaction _Blackboard)
       unfairField) ;; end deleteExcludeName
    (defun doClear() (error "accessDenied"))
    (defun findCriteriaName(name)
    ;; Return the measurement criteria with the specified unique name. 
       vars: (rowIndex rowCount nameString)
       (if (= measureST #void) (measureCriteriaLambda)) ;; Make sure we're initialized.
       (setq nameString (string name))
       (loop for rowIndex from 0 until measureST.rowCount do
          (if (= nameString measureST[Name: rowIndex])
              (return rowIndex))
          ) ;; end loop
       false) ;; end findCriteriaName
    (defun findCriteriaSpecification(specString)
    ;; Return the measurement criteria with the specified unique Specification. 
       vars: (rowIndex rowCount nameString)
       (if (= measureST #void) (measureCriteriaLambda)) ;; Make sure we're initialized.
       (loop for rowIndex from 0 until measureST.rowCount do
          (if (= specString measureST[Specification: rowIndex])
              (return rowIndex))
          ) ;; end loop
       false) ;; end findCriteriaSpecification
    (defun getColumnHeaders(startCol endCol) 
    ;; Return the History database valid column headers.
        vars:(i structLen dls cellValue)  
        ;;no value need as it is an RTTI
        (setq dls "")
        (setq structLen (ref (length _validFields)))
        (loop for i from 0 until structLen do
            (setq dls (append dls (ref _validFields [i 0]) (char 09)))
            ) ;;end i loop
        (append dls ""))
    (defun getCriteria(criteriaName)
    ;; Return the measurement criteria strategy specification string
    ;; saved under the specified unique name.
       vars:(rowIndex result)
       (if (= measureST #void) (measureCriteriaLambda)) ;; Make sure we're initialized.
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (setq result "")
           (setq result measureST[Specification: rowIndex])
           ) ;;end if
       result) ;; end getCriteria
    (defun getCriteriaLambda(criteriaName)
    ;; Return the measurement criteria strategy Lambda
    ;; saved under the specified unique name.
       vars:(rowIndex result)
       (if (= measureST #void) (measureCriteriaLambda)) ;; Make sure we're initialized.
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (setq result #void)
           (setq result measureST[Lambda: rowIndex])
           ) ;;end if
       result) ;; end getCriteriaLambda
    (defun loadCriteriaName()
    ;; Load list of all criteria names for display in the names list box. 
       vars: (rowIndex rowCount nameString)
       (if (= measureST #void) (measureCriteriaLambda)) ;; Make sure we're initialized.
       (setq nameString "")
       (loop for rowIndex from 0 until measureST.rowCount do
          (setq nameString (append nameString measureST[Name: rowIndex] (char 09)))
          ) ;; end loop
       nameString) ;; end loadCriteriaName
    (defun loadCondition() 
       vars: (conditionString)
       (setq conditionString "")
       (setq conditionString (append "and" (char 09) "or" (char 09) "<" (char 09) "<=" (char 09) "=" (char 09)))
       (setq conditionString (append conditionString ">" (char 09) ">=" (char 09) "<>" (char 09))))
    (defun loadCriteriaNotes(criteriaName) 
    ;; Return the measurement criteria notes string
    ;; saved under the specified unique name.
       vars:(rowIndex result) 
       (if (= measureST #void) (measureCriteriaLambda)) ;; Make sure we're initialized.
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (setq result "")
           (setq result measureST[Notes: rowIndex])
           ) ;;end if
       result) ;; end loadCriteriaNotes
    (defun loadExcludeNames(criteriaName)
    ;; Load list of all field names to be excluded from automatic strategies.
       vars: (rowIndex rowCount nameString unfairNames)
       (setq nameString "")
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex) (return nameString))
       (setq unfairNames measureST[Unfair: rowIndex])
       (setq rowCount (length unfairNames))
       (loop for rowIndex from 0 until rowCount do
          (setq nameString (append nameString unfairNames[rowIndex 0] (char 09)))
          ) ;; end loop
       nameString) ;; end loadExcludeNames
    (defun loadFieldName() 
       vars: (columnHeaders)
       (setq columnHeaders (myParent.getColumnHeaders 0 (myParent.refColCount))))
    (defun loadLogic() (append "and" (char 09) "or" (char 09)))
    (defun loadSort() 
       vars: (sortString)
       (setq sortString "")
       (setq sortString (append  sortString "avg" (char 09) "min" (char 09)))
       (setq sortString (append  sortString "max" (char 09) "std" (char 09) "criteria")))
    (defun loadUnfairFields(criteriaName)
    ;; Load Dictionary of all unfair field names to be excluded from automatic strategies.
       vars: (rowIndex)
       
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex) (return (setq unfairFields (makeDictionary))))
       (setq unfairFields measureST[Unfair: rowIndex])
       unfairFields) ;; end loadUnfairFields
    (defun loadValue() 
       vars: (valueString)
       (setq valueString ""))
    (defun ref2(col row) myTView[col row])
    (defun refColCount() myTView.colCount)
    (defun refColKeyness(col) myTView.colVector[col].colFormat)
    (defun refColName(col) myTView.colVector[col].name)
    (defun refColType(col) "G")
    (defun refRowCount() myTView.rowCount)
    (defun renameCriteria(oldName newName) 
    ;; Rename the specified criteria. Make sure the
    ;; new criteria name is unique.
       vars:(oldIndex newIndex)
       (beginTransaction _Blackboard) 
       (setq measureST (measureLoadStrategies))      
       (setq newIndex (findCriteriaName newName))
       (if (isNumber newIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "NameInUse")
           )) ;;end if
       (setq oldIndex (findCriteriaName oldName))
       (if (isBoolean oldIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "NoOldName")
           )) ;;end if
       (setq measureST[Name: oldIndex] (string newName))
       (setq _Blackboard.measureLambda measureST)
       (commitTransaction _Blackboard)
       newName) ;; end renameCriteria
    (defun runCriteriaLambda(criteriaName historyTableLambda)
    ;; Run the measurement criteria strategy Lambda,
    ;; saved under the specified unique name, to score
    ;; the specified history tableLambda.
       vars:(result algorithm)
       (setq algorithm (getCriteriaLambda criteriaName))
       (if (= algorithm #void)
           (setq result #void)
           (setq result (algorithm historyTableLambda))
           ) ;;end if
       result) ;; end runCriteriaLambda
    (defun saveCriteriaNotes(criteriaName newNotes) 
    ;; Save the specified criteria notes.
       vars:(rowIndex)
       (beginTransaction _Blackboard) 
       (setq measureST (measureLoadStrategies))      
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "NoOldName")
           )) ;;end if
       (setq measureST[Notes: rowIndex] (string newNotes))
       (setq _Blackboard.measureLambda measureST)
       (commitTransaction _Blackboard)
       true) ;; end saveCriteriaNotes
    (defun set2(col row newValue) (set myTView col row newValue))
    ;; Initialize the measurement Lambda
    ReInitialize::
    (setq measureST (measureLoadStrategies))
    (setq myParent (myself))
    (setq myTView (new Tableview: measureST))
    ) ;; end of measureCriteriaLambda




















;;**EXPORTKEY**:measureDummy
(defun measureDummy(ST detailRow)
;; *******************************************************************
;; summary:  This is a dummy measurement Lambda which simulates the
;;           kind of measurement Lambda which measureBuilder might return.
;; Args:     ST         History Tableview containing the selections.
;;           detailRow  screenLambda detail results row (see screenResultTables).
;; Return:   true   
;; Notes:    This dummy measurement Lambda implements the following command:
;;           avg,|3 Month Profit|
;; *******************************************************************
   vars:(rowCount rowIndex theCount)
   ;;  Find the number of selections.
   (setq rowCount ST.rowCount)
   (setq detailRow.|3 Month Profit| #void)
   (setq theCount 0)
   ;;  Initialize the measurement variable and all temporary variables.
   (define measure 0)
   ;;  Scan each selection and perform the measurement computations
   (loop for rowIndex from 0 until rowCount do
       ;; Compute statistics for each measure.
       (if (isNumber ST[|3 Month Profit|: rowIndex])
           (begin 
              (++ theCount)
              (+= measure ST[|3 Month Profit|: rowIndex])
              )) ;; end if
       ) ;; end summation loop
   ;;  Compute the final aggregation statistics for each measure.
   (if (> theCount 0) (/= measure theCount))
   ;;  Assign the measurement to the correct field in the.
   (setq detailRow.|3 Month Profit| measure)
   true)




























;;**EXPORTKEY**:measureLoadStrategies
(defun measureLoadStrategies(...)
;; *******************************************************************
;; Summary:  Load the specified History Measurement Strategies table
;;           of measurement formulas to be used in backtesting. Each 
;;           measurement strategy has a unique specification in the 
;;           language defined by the measureBuilder Lambda.
;; Note:     We use the blackboard to store the results of backtesting
;;           for many different history database screening strategies.
;; Args:     clearSW        (optional) True if the measure table is to be cleared.
;; Return:   measureTotals  The new screen Lambdas list of measurement
;;                          strategies for backtesting. 
;; *******************************************************************
   vars:(x y i n measureTable key)
   ;; The specifications for all history measurement strategies used in back
   ;; testing, are stored in the blackboard under the key "measureLambda". This
   ;; should be a Smarttable of the "measure" type (see screenResultTables). 
   (setq measureTable _Blackboard.measureLambda)
   ;; If the blackboard is empty, then we must create a measure smarttable
   ;; and enter an initial history measurement strategy for back testing. 
   (if (or (= measureTable #void) (> (argCount) 0))
       (setq measureTable (_defaultMeasureStrategies))
       ) ;; end if
   measureTable)































;;**EXPORTKEY**:multipleRegress
(defun multipleRegress(w)
;; *******************************************************************
;; summary:  Returns the sparse M coefficient vector for the factors 
;;           (count) with the best least squares fits of the variables.
;; Parms:    w:       The N by M+1 matrix representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;; Return:   v:       The M coefficient vector.
;; pvars:    count:   The maximum number of best fit factors to use.
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    pvars:((count 4))
    vars:(w wt v vx vy vr vs i n)
    ;; Extract the least squares error from multiple linear regressions
    (setq n (-1+ (length w[0])))
    (setq v (new Vector: n))
    (setq vy (matrixToVector w n))
    (loop for i from 0 until n do
        (setq vx (matrixToVector w i))
        (setq vr (regress vx vy))
        (setq v[i] vr[2]))
    ;; Sort the regression errors to find the smallest errors.
    (setq vs (sort (copyVector v) <))
    ;; Zero all but the best factors.
    (loop for i from 0 until n do
        (if (> v[i] vs[count]) 
            (matrixFillColumn w i 0)))
    ;; Perform a least squares regression on the remaining factors.
    (setq w (makeGausianMatrix w))
    (setq w (gausianEliminate w true))
    (setq v (gausianSubstitute w true))
    v)





























;;**EXPORTKEY**:multivariableRegress
(defun multivariableRegress(w)
;; *******************************************************************
;; summary:  Returns the dense M coefficient vector giving the coefficients
;;           with the best least squares fit of the variables.
;; Parms:    w:       The N by M+1 matrix representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;; Return:   v:       The M coefficient vector.
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    ;; Perform a least squares regression on all the factors.
    (setq w (makeGausianMatrix w))
    (setq w (gausianEliminate w true))
    (gausianSubstitute w true))
























;;**EXPORTKEY**:normalizeMatrix
(defun normalizeMatrix(mat)
;; *******************************************************************
;; summary:  Normalizes the column values in a matrix. Each matrix
;;           cell value is normalized by finding the high and low
;;           range for the value's column, then value is converted 
;;           into a fraction (0 ... 1) of it's column's high-low range.
;; Args:     mat:     A matrix with values to be normalized. The matrix
;;                    is stored in [row][column] order.
;; Return:   mat:     The normalized matrix (all values are fractions).
;; *******************************************************************
    vars:(i j colCount rowCount high low range)
    (setq rowCount (length mat))
    (setq colCount (length mat[0]))
    (loop for i from 0 until colCount do
        (setq high -999999999999999)
        (setq low 999999999999999)
        (loop for j from 0 until rowCount do
            (if (not (isNumber mat[j][i])) 
                (setq mat[j][i] 0))
            (if (< high mat[j][i]) 
                (setq high mat[j][i]))
            (if (> low mat[j][i]) 
                (setq low mat[j][i])))
        (setq range (- high low))
        (if (= range 0)
            (loop for j from 0 until rowCount do
                (setq mat[j][i] 1))
            (loop for j from 0 until rowCount do
                (setq mat[j][i] (/ (- mat[j][i] low) range)))))
    mat)

























;;**EXPORTKEY**:parseSentence
(defun parseSentence(aCmd)
;; *******************************************************************
;; summary:  Parses user entered natural language commands. An internal
;;           Dictionary of each command form is given. The command forms
;;           govern the parsing choices. Each positive choice has an action
;;           paired with the command.
;;           If no command form is recognized, false is returned.
;; Args:     aCmd                       The natural language command line.
;; Return:   Either false, or a list containing the new command.    
;; *******************************************************************
    vars:(name s n keys newCmd)
    pvars:(commands transformations _rules)
    ;; Function: During sentence parsing and transformation, attempt
    ;;           to match a sentence or sub-phrase with one of the
    ;;           rule patterns in the rules Dictionary variable (_rules).
    ;;           If no rule pattern is recognized, false is returned. 
    (defun applyRules(sexp)
       vars:(i n m dic cpy ret)
       (setq ret false)
       (setq m (length _rules))
       (loop for i from 0 until m do
           (setq dic (new Dictionary:))
           (if (isMatch _rules[i 0] sexp dic)
               (begin 
                  (setq cpy (copy _rules[i]))
                  (setq ret (bind cpy dic)))))
       ret)
    ;; Create the Dictionary of command forms (if necessary).
    (if (= commands #void)
        (begin
           (setq commands (new Dictionary:))
           (setq commands['("what" "is" ("your" ("name")))] 
                    '(append "I am called Ma."))
           (setq commands['("who" "are" "you")] 
                    '(append "I am called Ma."))
           (setq commands['("what" "are" "you" ("called"))] 
                    '(append "I am called Ma."))
           (setq commands['("show" "me" ($x))] 
                    '(append "The variable " $x " contains, " (getGlobalvalue (makeSymbol $x)) "."))
           (setq commands['("show" ($x))] 
                    '(append "The variable " $x " contains, " (getGlobalvalue (makeSymbol $x)) "."))
           (setq commands['("what" "is" ($x))] 
                    '(append "The variable " $x " contains, " (getGlobalvalue (makeSymbol $x)) "."))
           (setq commands['("does" ($x) "equal" ($y))] 
                    '(append 
                               (if (isEqual 
                                     (getGlobalvalue (makeSymbol $x)) 
                                     (getGlobalvalue (makeSymbol $y)))
                                   "Yes."
                                   "No.")))
           (setq commands['("is" ($x) "equal" "to" ($y))] 
                              '(append 
                               (if (isEqual 
                                     (getGlobalvalue (makeSymbol $x)) 
                                     (getGlobalvalue (makeSymbol $y)))
                                   "Yes."
                                   "No.")))
                                )) ;; end if
       ;; Create the Dictionary of transformations (if necessary).
    (if (= transformations #void)
        (begin
           (setq transformations (new Dictionary:))
           (setq transformations['("variable" $x)] '($x))
           (setq transformations['($x "variable")] '($x))
           (setq transformations['("the" ($x))]  '($x))
                                )) ;; end if
    ;; Convert the sentence into a list.
    (setq s (sentenceToList aCmd)) 
    ;; See if we can transform the sentence sub-phrases with simpler phrases.
    (setq _rules transformations)
    (setq s (morph s applyRules))
    ;; See if we can match the sentence with a command form.
    (setq _rules commands)
    (applyRules s)) ;; end of parseSentence





























;;**EXPORTKEY**:prodCriteriaLambda
(defun prodCriteriaLambda()
;; *******************************************************************
;; Summary: This Lambda is in charge of Production database selection
;;          Screening Strategies which are run against the _LegacyDB.
;; Args:    none
;; Return:  myTView     The tableview of the selection criteria. 
;; *******************************************************************
    pvars:(selectST                  ;; The selection criteria Smarttable
           myParent                  ;; The parent Lambda object reference
           myTView                   ;; The selection criteria Tableview
           nameSerial                ;; A serial number for generating automated names
           sortLambda                ;; The sort lambda value for use in sorting criteria
           (maxCriteria 1000)        ;; Maximum selection criteria allowed in blackboard
           ;; Child Lambdas
           addNewCriteria            ;; Add the specified new criteria to the blackboard
           Checkin
           deleteCriteria            ;; Delete the specified criteria from the blackboard
           doClear
           dropAutoCriteria          ;; Remove all machine generated criteria from the Blackboard
           findCriteriaName          ;; Return the selection criteria with the specified unique name. 
           findCriteriaSpecification ;; Return the selection criteria with the specified unique Specification. 
           getColumnHeaders          ;; Return the Legacy database valid column headers
           getCriteria               ;; Return the selection criteria strategy specification string
           loadCondition  
           loadCriteriaName          ;; Load list of all criteria names for display in the names list box. 
           loadCriteriaNotes         ;; Return the selection criteria notes string
           loadFieldName  
           loadLogic 
           loadSort  
           loadValue 
           ref2
           refColCount
           refColKeyness
           refColName
           refColType
           refRowCount
           renameCriteria            ;; Rename the specified criteria
           saveCriteriaNotes         ;; Save the specified criteria notes
           set2
           ) ;; end of persistant variables
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> ref2 #void) (goto ReInitialize:))
    ;; Initialize the inline child Lambdas.
    (defun addNewCriteria(newSpec newName newNotes)
    ;; Add the specified new criteria to the blackboard.
       vars:(rowIndex nameIndex)
       ;; If no user name, generate a new name automatically.
       (if (= newName #void)
           (setq newName (append "_Strategy_" (now)))
           ) ;; end if
       ;; Load selection strategies table and set up long transaction.
       (beginTransaction _Blackboard) 
       (setq selectST (prodLoadStrategies))      
       ;; See if the selection criteria table is already full.
       (if (>= selectST.rowCount maxCriteria)
           (begin
              (abortTransaction _Blackboard)
              (error "databaseFull")
           )) ;;end if
       ;; See if the user criteria specification is already in use.
       (setq rowIndex (findCriteriaSpecification newSpec))
       (if (isNumber rowIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "CriteriaInUse")
           )) ;;end if
       ;; See if the user criteria name is already in use.
       (setq nameIndex (findCriteriaName newName))
       (if (isNumber nameIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "NameInUse")
           )) ;;end if
       ;; Add the selection criteria and its detail results table (if any).
       (deleteView myTView)
       (setq myTView #void)
       (setq rowIndex selectST.rowCount)
       (addRow selectST 1 rowIndex)
       (setq selectST[Specification: rowIndex] newSpec)
       (setq selectST[Notes: rowIndex] newNotes)
       (setq selectST[Name: rowIndex] newName)
       (setq _Blackboard.prodLambda selectST)
       (commitTransaction _Blackboard)
       (setq myTView (^new Tableview: selectST))
       newName) ;; end addNewCriteria
    (defun Checkin(title) (writeln "Checked in " title))
    (defun deleteCriteria(criteriaName) 
    ;; Delete the specified criteria from the blackboard.
       vars:(rowIndex detailTableKey)
       (beginTransaction _Blackboard) 
       (setq selectST (prodLoadStrategies))      
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "UnknownName")
           )) ;;end if
       ;; Delete the selection criteria and its detail results table (if any).
       (setq detailTableKey (append "prodLambda:" selectST[rowIndex].Specification))
       (setq _Blackboard[detailTableKey] #void)
       (deleteView myTView)
       (setq myTView #void)
       (deleteRow selectST 1 rowIndex)
       (setq _Blackboard.prodLambda selectST)
       (commitTransaction _Blackboard)
       (setq myTView (^new Tableview: selectST))
       true) ;; end saveCriteriaNotes
    (defun doClear() (setq selectST (new Smarttable:)))
    (defun findCriteriaName(name)
    ;; Return the selection criteria with the specified unique name. 
       vars:(rowIndex rowCount nameString)
       (if (= selectST #void) (selectCriteriaLambda)) ;; Make sure we're initialized.
       (setq nameString (string name))
       (loop for rowIndex from 0 until selectST.rowCount do
          (if (= nameString selectST[Name: rowIndex])
              (return rowIndex))
          ) ;; end loop
       false) ;; end findCriteriaName
    (defun findCriteriaSpecification(specString)
    ;; Return the selection criteria with the specified unique Specification. 
       vars:(rowIndex rowCount nameString)
       (if (= selectST #void) (selectCriteriaLambda)) ;; Make sure we're initialized.
       (loop for rowIndex from 0 until selectST.rowCount do
          (if (= specString selectST[Specification: rowIndex])
              (return rowIndex))
          ) ;; end loop
       false) ;; end findCriteriaSpecification
    (defun getColumnHeaders(startCol endCol) 
    ;; Return the Legacy database valid column headers.
        vars:(i structLen dls cellValue)  
        ;;no value need as it is an RTTI
        (setq dls "")
        (setq structLen (ref (length _validFields)))
        (loop for i from 0 until structLen do
            (setq dls (append dls (ref _validFields [i 0]) (char 09)))
            ) ;;end i loop
        (append dls ""))
    (defun getCriteria(criteriaName)
    ;; Return the selection criteria strategy specification string
    ;; saved under the specified unique name.
       vars:(rowIndex result)
       (if (= selectST #void) (selectCriteriaLambda)) ;; Make sure we're initialized.
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (setq result "")
           (setq result selectST[Specification: rowIndex])
           ) ;;end if
       result) ;; end getCriteria
    (defun loadCondition() 
       vars:(conditionString)
       (setq conditionString "")
       (setq conditionString (append "and" (char 09) "or" (char 09) "<" (char 09) "<=" (char 09) "=" (char 09)))
       (setq conditionString (append conditionString ">" (char 09) ">=" (char 09) "<>" (char 09))))
    (defun loadCriteriaName(...)
    ;; Load list of all criteria names for display in the names list box. 
       vars:(rowIndex rowCount nameString measureName)
       ;; Make sure we're initialized.
       (if (= selectST #void) (selectCriteriaLambda))
       (findAll myTView)
       ;; Load optional sort argument. If present,
       ;; use it to presort the selection criteria names.
       (if (= (argCount) 1)
           (begin
              (setq sortLambda.Pv.fieldName (symbol (argFetch 0)))
              (sort myTView sortLambda)
              )) ;; end if
       (setq nameString "")
       (loop for rowIndex from 0 until myTView.rowCount do
          (setq nameString (append nameString myTView[Name: rowIndex] (char 09)))
          ) ;; end loop
       nameString) ;; end loadCriteriaName
    (defun loadCriteriaNotes(criteriaName) 
    ;; Return the selection criteria notes string
    ;; saved under the specified unique name.
       vars:(rowIndex result) 
       (if (= selectST #void) (selectCriteriaLambda)) ;; Make sure we're initialized.
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (setq result "")
           (setq result selectST[Notes: rowIndex])
           ) ;;end if
       result) ;; end loadCriteriaNotes
    (defun loadFieldName() 
       vars:(columnHeaders)
       (setq columnHeaders (myParent.getColumnHeaders 0 (myParent.refColCount))))
    (defun loadLogic() (append "and" (char 09) "or" (char 09)))
    (defun loadSort() 
       vars:(sortString)
       (setq sortString "")
       (setq sortString (append  sortString "all" (char 09) "top" (char 09)))
       (setq sortString (append  sortString "bottom" (char 09) "omit" (char 09) "rule")))
    (defun loadValue() 
       vars:(valueString)
       (setq valueString ""))
    (defun ref2(col row) myTView[col row])
    (defun refColCount() myTView.colCount)
    (defun refColKeyness(col) myTView.colVector[col].colFormat)
    (defun refColName(col) myTView.colVector[col].name)
    (defun refColType(col) "G")
    (defun refRowCount() myTView.rowCount)
    (defun renameCriteria(oldName newName) 
    ;; Rename the specified criteria. Make sure the
    ;; new criteria name is unique.
       vars:(oldIndex newIndex)
       (beginTransaction _Blackboard) 
       (setq selectST (prodLoadStrategies))      
       (setq newIndex (findCriteriaName newName))
       (if (isNumber newIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "NameInUse")
           )) ;;end if
       (setq oldIndex (findCriteriaName oldName))
       (if (isBoolean oldIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "UnknownName")
           )) ;;end if
       (setq selectST[Name: oldIndex] (string newName))
       (setq _Blackboard.prodLambda selectST)
       (commitTransaction _Blackboard)
       newName) ;; end renameCriteria
    (defun saveCriteriaNotes(criteriaName newNotes) 
    ;; Save the specified criteria notes.
       vars:(rowIndex)
       (beginTransaction _Blackboard) 
       (setq selectST (prodLoadStrategies))      
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "UnknownName")
           )) ;;end if
       (setq selectST[Notes: rowIndex] (string newNotes))
       (setq _Blackboard.prodLambda selectST)
       (commitTransaction _Blackboard)
       true) ;; end saveCriteriaNotes
    (defun set2(col row newValue) (set myTView col row newValue))
    ;; Initialize the selection Lambda
    ReInitialize::
    (setq selectST (prodLoadStrategies))
    (setq myParent (myself))
    (setq myTView (new Tableview: selectST))
    (setq sortLambda (eval '(lambda (x y) pvars:(fieldName) (> x[fieldName] y[fieldName]))))
    ) ;; end of prodCriteriaLambda




















;;**EXPORTKEY**:prodLoadStrategies
(defun prodLoadStrategies()
;; *******************************************************************
;; Summary:  Load the specified Production Screening Strategy to the
;;           screen Lambdas list of strategies. Each strategy has a unique
;;           specification in the language defined by the screenBuilder
;;           procedure.
;; Note:     We use the blackboard to store the results of backtesting
;;           for many different history database screening strategies.
;; Args:     none
;; Return:   screenTotals   The new screen Lambdas list of strategies
;;                          for backtesting. 
;; *******************************************************************
   vars:(x y i n screenTotals key strategy)
   ;; Obtain the specification of the strategy to add.
   (if (>= (argCount) 1)
       (setq strategy (argFetch 0))
       (setq strategy #void))
   ;; The results for all history screening strategies back tested so far
   ;; are stored in the blackboard under the key "screenLambda". This
   ;; should be a Smarttable of the totals type (see screenResultTables). 
   (setq screenTotals _Blackboard.prodLambda)
   ;; If the blackboard is empty, then we must create a totals smarttable
   ;; and enter an initial history screening strategy for back testing. 
   (if (= screenTotals #void)
       (setq screenTotals (_defaultProdStrategies))
       (setq _Blackboard.prodLambda screenTotals)
       ) ;; end if
   screenTotals)































;;**EXPORTKEY**:projectLambda
(defun projectLambda()
;; *******************************************************************
;; summary:  Manages all project metadata for AB/M application.
;; 
;; Args:     none
;; Return:   true
;; *******************************************************************
  pvars:(myParent                ;; Holds the parent Lambda object reference
         currentProject          ;; Currently loaded project record
         currentDirectory        ;; Currently loaded project directory
         currentMiner            ;; Currently loaded miner object
         ;; slot constants
         (projectName 0)         ;; Slot number of name in project structure
         (projectTable 1)        ;; Slot number of table in project structure
         (projectViews 2)        ;; Slot number of views in project structure
         (projectScorers 3)      ;; Slot number of scorers in project structure
         (projectFilters 4)      ;; Slot number of filters in project structure
         (projectMiners 5)       ;; Slot number of miners in project structure
         (minerName 0)           ;; Slot number of name in miner structure
         (minerDate 1)           ;; Slot number of creation date in miner structure
         (minerTime 2)           ;; Slot number of mining time in miner structure
         (minerGoal 3)           ;; Slot number of goal Lambda in miner structure
         (minerBias 4)           ;; Slot number of fitness bias in miner structure
         (minerStat 5)           ;; Slot number of fitness stat in miner structure
         (minerFactory 6)        ;; Slot number of factory list in miner structure
         (minerBoard 7)          ;; Slot number of blackboard in miner structure
         (minerHistory 8)        ;; Slot number of history in miner structure
         (minerFilters 9)        ;; Slot number of filters in miner structure
         (minerScorers 10)       ;; Slot number of scorers in miner structure
         (minerViews 11)         ;; Slot number of test datasets in miner structure
         (minerNoResult 12)      ;; Slot number of drop no-result filters option
         ;;methods
         addProject              ;; Adds a project
         addProjectObject        ;; Adds an object to the specified project
         addMinerObject          ;; Adds an object to the specified miner
         bindSlotObject          ;; Binds an object to a slot
         clearProjects           ;; Removes all projects
         createDefault           ;; Creates default project
         createMinerStructure    ;; Creates the structure used to store miner info
         createProjectStructure  ;; Creates the structure used to store project info
         createViewStructure     ;; Creates the structure used to store view info
         deleteMinerObject       ;; Deletes an object from a miner
         deleteProject           ;; Deletes a project
         deleteProjectObject     ;; Deletes an object from a project
         getMiner                ;; Returns a particular miner structure and sets current miner
         getMinerObjectList      ;; Returns a vector of miner objects
         getObjectList           ;; Returns a vector of project objects
         getProject              ;; Returns a particular project structure and sets current structure
         getProjectList          ;; Returns a vector of project names
         initProjectDirectory    ;; Initializes project directory   
         saveProjectDirectory    ;; Saves project directory
         updateMiner             ;; Update a miner
         updateProject           ;; Update a project 
         )   ;;end of persistent variables
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistent storage of the original Lambda.
    (if (<> addProject #void) (goto Continue:))
    ;; Initialize the inline child Lambdas.
    (defun addProject(projectName projectTable)
       ;; *******************************************************************
       ;; summary:  Add a new project. 
       ;;
       ;; Args:     projectName    name of project
       ;;           projectTable   primary table associated with project
       ;;
       ;; Return:   True
       ;;               
       ;; *******************************************************************
       vars:(
             tableList            ;; Table list structure
             i                    ;; Index           
            ) 
       ;;
       ;;make sure Directory is initialized
       (initProjectDirectory)
       ;;
       ;; Test for #void args 
       (if (or (= projectName #void) (= projectTable #void))
           (error "invalidArg"))
       ;;
       ;; Test that the primary table exists
       ;;
       ;; get the table list
       (setq tableList (dataMineLib.getObjectNames primary:))  
       (setq i 0) ;; initialize the index variable
       ;; scan the table list for the specified name
       (while (and (<> tableList[i] projectTable) (< i (length tableList))) do  
           (++ i)
       )
       ;; Report the error if the table does not exist
       (if (= i (length tableList)) 
           (error "unknownTable"))
       ;;
       ;; Retrieve the project from the Dictionary to test for duplicates
       (if (<> (setq currentProject currentDirectory[projectName]) #void)
           (error "projExists"))
       ;;
       ;; Create an empty project structure
       (createProjectStructure)
       ;;
       ;; Initialize project fields
       (setq currentProject.name projectName)
       (setq currentProject.table projectTable)
       ;;
       ;; Save the new project to the directory
       (setq currentDirectory[projectName] currentProject)
       ;;
       ;; Save the modified directory
       (saveProjectDirectory)
       ;;
       true) ;; end of addProject
    (defun addProjectObject(projectName objectType objectValue)
       ;; *******************************************************************
       ;; summary:  Add a new object to a project
       ;;
       ;; Args:     projectName     name of the project to add to
       ;;           objectType      type of the object to add. Valid types are
       ;;                               filterLambda:
       ;;                               goalLambda:
       ;;                               scoreLambda:
       ;;                               miner:
       ;;                               view:
       ;;           objectValue     the object to add
       ;;
       ;; Return:   True
       ;;               
       ;; *******************************************************************
       ;; 
       ;; Test to see if project is already open
       (if (<> currentProject.name projectName) 
          (begin
             ;; Initialize the directory
             (initProjectDirectory)
             ;;
             ;; Retrieve the export layout from the Dictionary
             (if (= (setq currentProject currentDirectory[projectName]) #void)
                (error "unknownProject"))
          )
       )    
       ;;
       ;; Evaluate the object type and bind appropriately
       (cond ((= objectType filterLambda:) 
                  (bindSlotObject projectFilters project: objectValue "filterExists")
                )
             ((= objectType scoreLambda:)
                  (bindSlotObject projectScorers project: objectValue "scorerExists")
                )
             ((= objectType miner:) 
                  (begin
                     ;; Initialize a new miner object
                     (createMinerStructure)
                     ;; Set initial values
                     (setq currentMiner.name (symbol objectValue))
                     (setq currentMiner.creationDate (string (date (now))))
                     (setq currentMiner.miningTime 0)
                     ;; Bind to slot in project
                     (bindSlotObject projectMiners projectMiner: objectValue "minerExists")
                  )
                )
             ((= objectType view:) 
                  (bindSlotObject projectViews project: objectValue "viewExists")
                )
          )
       ;;
       ;; Save export layout to dictionary
       (setq currentDirectory[projectName] currentProject)
       (saveProjectDirectory)
       true) ;; end of addProjectObject
    (defun addMinerObject(minerName objectType objectValue)
       ;; *******************************************************************
       ;; summary:  Add a new object to a miner
       ;;
       ;; Args:     minerName       name of the target miner
       ;;           objectType      type of the object to add. Valid types are
       ;;                               filterLambda:
       ;;                               scoreLambda:
       ;;                               view:
       ;;           objectValue     the object to add
       ;;
       ;; Return:   True
       ;;               
       ;; *******************************************************************
       ;;
       ;; Current project must be initialized
       (if (= currentProject #void)
          (error "projNotOpen"))
       ;;
       ;; Retrieve the miner from the Dictionary
       (if (= (setq currentMiner currentProject.miners[(symbol minerName)]) #void)
           (error "unknownMiner"))
       ;;
       ;; Evaluate the object type and bind appropriately
       (cond ((= objectType filterLambda:) 
                  (bindSlotObject minerFilters miner: objectValue "filterExists")
                )
             ((= objectType scoreLambda:)
                  (bindSlotObject minerScorers miner: objectValue "scorerExists")
                )
             ((= objectType view:) 
                  (bindSlotObject minerViews miner: objectValue "viewExists")
                )
          )
       ;;
       ;; Save update miner to project
       (setq currentProject.miners[(symbol currentMiner.name)] currentMiner)
       (setq currentDirectory[(symbol currentProject.name)] currentProject)
       (saveProjectDirectory)
       true) ;; end of addMinerObject
    (defun bindSlotObject(slotNum receiverType objectValue errorString)
       ;; constructs or extends an object structure at the specified
       ;; slot number of the current project or current miner 
       ;;
       (if (= receiverType project:) 
          (begin
             (if (= currentProject[slotNum] #void) 
                (setq currentProject[slotNum] (makeStructure (symbol objectValue) objectValue))
             else
                (if (= currentProject[slotNum][(symbol objectValue)] #void) 
                   (setq currentProject[slotNum][(symbol objectValue)] objectValue)
                else
                   (error errorString)
                )
             )
          )
        else
          (if (= receiverType miner:)
             ;; receiver is a miner object
             (begin
                (if (= currentMiner[slotNum] #void) 
                   (setq currentMiner[slotNum] (makeStructure (symbol objectValue) objectValue))
                else
                   (if (= currentMiner[slotNum][(symbol objectValue)] #void) 
                      (setq currentMiner[slotNum][(symbol objectValue)] objectValue)
                   else
                      (error errorString)
                   )
                )
             ) 
          else
             ;; project gets a new miner object
             (begin
                (if (= currentProject[slotNum] #void) 
                   (setq currentProject[slotNum] (makeStructure (symbol objectValue) currentMiner))
                else
                   (if (= currentProject[slotNum][(symbol objectValue)] #void) 
                      (setq currentProject[slotNum][(symbol objectValue)] currentMiner)
                   else
                      (error errorString)
                   )
                )
             ) 
          )
       )
       true) ;; end of bindSlotObject
    (defun clearProjects()
       ;;clears the project directory
       ;;
       ;;initialize project directory
       (initProjectDirectory)
       ;;
       ;;erase current project
       (createProjectStructure)
       ;;
       ;;erase project directory
       (setq currentDirectory #void)
       ;;
       ;;write to repository
       (saveProjectDirectory)
       true) ;; eop clearProjects
    (defun createMinerStructure()
       (setq currentMiner (makeStructure name:          #void  ;; symbol of the name
                                         creationDate:  #void  ;; timestamp
                                         miningTime:    #void  ;; number representing the time spent mining
                                         goalLambda:     #void  ;; String of goal Lambda name
                                         scoreBias:     #void  ;; Symbol of bias
                                         scoreStat:     #void  ;; Symbol of fitness stat
                                         factoryList:   #void  ;; Structure of factory Lambdas
                                         blackBoard:    #void  ;; String of blackboard (this goes away in V2)
                                         history:       #void  ;; String of history table (this goes away in V2)
                                         filters:       #void  ;; Structure of filter Lambdas
                                         scorers:       #void  ;; Structure of scorers
                                         views:         #void  ;; Structure of view tables 
                                         dropNoResult:  #void  ;; Discard no-result filter Lambdas (true or false)
                           )
       )
       true) ;; eop createMinerStructure
    (defun createProjectStructure()
       (setq currentProject (makeStructure name:        #void  ;; symbol of the name
                                           table:       #void  ;; string of the table
                                           views:       #void  ;; structure of views 
                                           scorers:     #void  ;; structure of scoring Lambdas
                                           filters:     #void  ;; structure of filter Lambdas
                                           miners:      #void  ;; structure of miners
                            )
       ) 
       true) ;; eop createProjectStructure
    (defun createViewStructure()
       (makeStructure name:        #void 
                      filter:      #void
       )) ;; eop createViewStructure
    (defun deleteMinerObject(minerName objectType objectKey projectName) 
       ;;
       ;;initialize directory
       (if (= currentDirectory #void) (initProjectDirectory))
       ;;
       ;;attempt to load specified project 
       (if (<> projectName #void) (begin
          (setq currentProject currentDirectory[projectName]))
          ;;
          ;;error if project not found
          (if (= currentProject #void)
             (error "unknownProject")
          )
       )
       ;;attempt to load specified miner
       (getMiner minerName)
       ;;
       ;;error if project not found
       (if (= currentMiner.name #void)
          (error "unknownMiner")
       )
       ;;
       ;; Evaluate the object type and delete appropriately
       (cond ((= objectType filterLambda:) 
                  (if (<> currentMiner.filters[objectKey] #void) 
                     (setq currentMiner.filters (delete objectKey currentMiner.filters))
                  )
                )
             ((= objectType scoreLambda:)
                  (if (<> currentMiner.scorers[objectKey] #void) 
                     (setq currentMiner.scorers (delete objectKey currentMiner.scorers))
                  )
                )
             ((= objectType view:) 
                  (if (<> currentMiner.views[objectKey] #void) 
                     (setq currentMiner.views (delete objectKey currentMiner.views))
                  )
                )
          )
       ;;
       ;; Save the miner
       (updateMiner minerName currentMiner)
       true) ;; end of deleteMinerObject
    (defun deleteProject(projectName)
       ;; *******************************************************************
       ;; summary:  Delete a project
       ;;
       ;; Args:     projectName    name of project to delete
       ;;           
       ;;
       ;; Return:   True
       ;;               
       ;; *******************************************************************
       ;;
       ;; Initialize the directory
       (initProjectDirectory)
       ;;
       ;; Retrieve the export layout from the Dictionary
       (if (= (setq currentProject currentDirectory[projectName]) #void)
           (error "unknownProject"))
       ;;
       ;; Clear the currentProject
       (setq currentProject #void)
       ;;
       ;; Delete the named project from the directory
       (setq currentDirectory (delete projectName currentDirectory))
       (saveProjectDirectory)    
       ;;
       true) ;; end of deleteProject
    (defun deleteProjectObject(projectName objectType objectKey) 
       ;;initialize directory
       (initProjectDirectory)
       ;;
       ;;attempt to load specified project 
       (setq currentProject currentDirectory[projectName])
       ;;
       ;;error if project not found
       (if (= currentProject #void)
          (error "unknownProject")
       )
       ;;
       ;; Evaluate the object type and delete appropriately
       (cond ((= objectType filterLambda:) 
                  (if (<> currentProject.filters[objectKey] #void) 
                     (setq currentProject.filters (delete objectKey currentProject.filters))
                  )
                )
             ((= objectType scoreLambda:)
                  (if (<> currentProject.scores[objectKey] #void) 
                     (setq currentProject.scores (delete objectKey currentProject.scores))
                  )
                )
             ((= objectType miner:) 
                  (if (<> currentProject.miners[objectKey] #void) 
                     (setq currentProject.miners (delete objectKey currentProject.miners))
                  )
                )
             ((= objectType view:) 
                  (if (<> currentProject.views[objectKey] #void) 
                     (setq currentProject.views (delete objectKey currentProject.views))
                  )
                )
          )
       ;;
       ;; Save project to directory
       (setq currentDirectory[projectName] currentProject)
       (saveProjectDirectory)
       true) ;; end of deleteProjectObject
    (defun getObjectList(projectName objectType) 
       ;;
       ;; Summary: Return the list of project objects in a vector
       ;;
       ;; Args:    projectName    name of the project to get from
       ;;          objectType     type of the objects to return
       ;;          Valid types are: filterLambda:
       ;;                           scoreLambda:
       ;;                           table:
       ;;                           miner:
       ;;                           view:
       ;;
       vars: (i objectList sourceList recCount)
       ;;
       ;;
       (initProjectDirectory)
       ;;
       ;;attempt to load specified project 
       (setq currentProject currentDirectory[projectName])

       ;;error if project not found
       (if (= currentProject #void)
          (error "unknownProject")
       )
       ;;
       ;; Evaluate the object type and set reference variable
       ;; Return immediately if singleton property
       (cond 
          ((= objectType filterLambda:) 
             (setq sourceList currentProject.filters)
          )
          ((= objectType scoreLambda:)
            (setq sourceList currentProject.scores)
          )
          ((= objectType table:)
            (begin
               (setq objectList (new Vector: 0))
               (return (setq objectList[0] currentProject.table))
            )
          )
          ((= objectType miner:) 
            (setq sourceList currentProject.miners)
          )
          ((= objectType view:) 
            (setq sourceList currentProject.views)
          )
          (else (error "unknownType"))
       )
       ;;
       ;; Process object structures to build return vector
       ;;
       (setq objectList (new Vector: 0)) 
       (setq objectList[0] #void)
       (if (<> sourceList #void) (begin
          (setq recCount (length sourceList))
          (if (or (= objectType filterLambda:) (= objectType scoreLambda:))
             (loop for i from 0 until recCount do
                 (setq objectList[i] sourceList[i]) 
             ) ;;end i loop
           else          
             (loop for i from 0 until recCount do
                 (setq objectList[i] sourceList[i].name) 
             ) ;;end i loop
          )
       ))
       objectList) ;; eop getObjectList
    (defun getMiner(minerName)
       ;;
       ;; Summary: returns a miner object from the open project
       ;;
       ;; Args:    minerName   Miner object to return. 
       ;;
       ;; Test that a project is open
       (if (= currentProject #void) 
          (error "projNotOpen")
       )
       ;;
       ;; Retrieve the specified miner object or report error
       (if (= (setq currentMiner currentProject.miners[(symbol minerName)]) #void)
          (error "unknownMiner"))
       ;;
       currentMiner) ;; eop getMiner
    (defun getMinerObjectList(minerName objectType) 
       ;;
       ;; Summary: Return the list of project objects in a vector
       ;;
       ;; Args:    minerName    name of the project to get from
       ;;          objectType   type of the objects to return
       ;;                       valid types are:
       ;;                              filterLambda:
       ;;                              scoreLambda:
       ;;                              goalLambda:
       ;;                              view:
       ;;                              factoryLambda:
       ;;                              creationDate:
       ;;                              miningTime:
       ;;                              dropNoResult:
       ;;
       vars: (i j objectList sourceList recCount)
       ;;
       ;;attempt to load specified miner
       (if (= (getMiner minerName) #void) (return (new Vector: 0)))
       ;;
       ;; Evaluate the object type and set reference variable
       ;; Return immediately if singleton property
       (cond 
          ((= objectType filterLambda:) 
             (setq sourceList currentMiner.filters)
          )
          ((= objectType scoreLambda:)
            (setq sourceList currentMiner.scorers)
          )
          ((= objectType goalLambda:)
            (begin 
               (setq objectList (new Vector: 0))
               (return (setq objectList[0] currentMiner.goalLambda))
            )
          )
          ((= objectType creationDate:)
            (begin 
               (setq objectList (new Vector: 0))
               (return (setq objectList[0] currentMiner.creationDate))
            )
          )
          ((= objectType miningTime:)
            (begin 
               (setq objectList (new Vector: 0))
               (return (setq objectList[0] currentMiner.miningTime))
            )
          )
          ((= objectType dropNoResult:)
            (begin 
               (setq objectList (new Vector: 0))
               (return (setq objectList[0] currentMiner.dropNoResult))
            )
          )
          ((= objectType view:) 
            (setq sourceList currentMiner.views)
          )
          ((= objectType factoryLambda:) 
            (setq sourceList currentMiner.factoryList)
          )
          (else (error "unknownType"))
       )
       ;;
       ;; Process object structures to build return vector
       ;;
       (setq recCount (length sourceList))
       ;; (setq objectList #(#void)) ;; MFK - debugger shows this constant has been overwritten
       (setq objectList (new Vector: 0))
       ;; This initialization ensures that vectorToDelimitedString can accept 
       ;; the return value of getMinerObjectList as an argument
       (setq objectList[0] #void)
       (if (or (= objectType filterLambda:) (= objectType scoreLambda:) (= objectType view:))
          (loop for i from 0 until recCount do
              (setq objectList[i] sourceList[i]) 
          ) ;;end i loop
        else          
          (if (= objectType factoryLambda:) 
             (begin 
                (setq j 0)  ;; Initialize target index
                (loop for i from 0 until recCount do 
                    (begin
                       (setq objectList[j] sourceList[i 0])
                       (setq objectList[(+ j 1)] sourceList[i 1])
                       (+= j 2)  ;; Increment target index
                    ) 
                ) ;;end i loop
             )
          else
             (loop for i from 0 until recCount do
                 (setq objectList[i] sourceList[i].name) 
             ) ;;end i loop
          )
       )
       objectList) ;; eop getObjectList
    (defun getProject(projectName)
       ;; returns the record of a project and sets currentProject
       ;;
       ;; Assure directory object is initialized
       (initProjectDirectory)
       ;;
       ;; Retrieve specified project
       (if (= (setq currentProject currentDirectory[projectName]) #void)
          (if (<> projectName default:) 
             (error "unknownProject")
          else
             ;; create default project structure
             (createDefault default:)
          )
       )
       ;;
       currentProject) ;; eop getProject
    (defun getProjectList() 
       ;; Return the list of project names in a vector
       vars: (i projectList recCount)
       (initProjectDirectory)
       (setq projectList (new Vector: 0))
       (setq recCount (length currentDirectory))
       (loop for i from 0 until recCount do
           (setq projectList[i] currentDirectory[i 1].name) 
       ) ;;end i loop
       projectList) ;; eop getObjectList
    (defun initProjectDirectory()
       ;; initializes project directory
       (if (= _MetadataDB[projectDirectory:] #void)
          (begin
             (beginTransaction _MetadataDB)
             (setq _MetadataDB[projectDirectory:] (^new Directory:))
             (commitTransaction _MetadataDB)
          )
       )
       (beginTransaction _MetadataDB)
       (setq currentDirectory _MetadataDB[projectDirectory:])
       (abortTransaction _MetadataDB)
       true) ;; eop initProjectDirectory
    (defun saveProjectDirectory()
       ;;saves project directory
       (beginTransaction _MetadataDB)
       (setq _MetadataDB[projectDirectory:] currentDirectory)
       (commitTransaction _MetadataDB)
       true)
    (defun updateMiner(minerName minerObject)
       ;;
       (setq currentMiner minerObject)
       (setq currentProject.miners[minerName] currentMiner)
       (updateProject currentProject.name currentProject)
       true)
    (defun updateProject(projectName projectObject)
       ;;
       (initProjectDirectory)
       ;;
       (setq currentProject projectObject)
       ;;
       ;; Save project to directory
       (setq currentDirectory[projectName] currentProject)
       (saveProjectDirectory)
       true) ;; end of updateProject
    ;; Initialize the project Lambda
    Continue::
    (setq myParent (myself))
    (getProject default:)
    true) ;; end of projectLambda











;;**EXPORTKEY**:projectLambda:createDefault
(defChildLambda projectLambda:createDefault(projectName)
   ;;
   vars:(result i limit)
   ;;
   (addProject projectName (string (ref (dataMineLib.getObjectNames primary:) 0)))
   ;;
   ;; Add a default miner that surfaces the 1.5 objects
   (addProjectObject default: miner: |John Henry|:)
   ;;
   ;; Add all available views
   (setq result (dataMineLib.getObjectNames view:))
   (setq limit (length result))
   (loop for i from 0 until limit do 
      (begin
          (addProjectObject default: view: result[i])
          (addMinerObject |John Henry|: view: result[i])
      )
   )
   ;;
   ;; Add all available scorers
   (setq result (stringToVector (dataMineLib.getTableChildTab scoreLambda: #void) #\tab))
   (setq limit (length result))
   (loop for i from 0 until limit do 
      (begin
          (addProjectObject default: scoreLambda: result[i])
          (if (<> i 0) 
             (addMinerObject |John Henry|: scoreLambda: result[i])
          else
             (begin
                 (setq currentMiner[goalLambda:] result[i])
                 (setq currentMiner[scoreStat:] sum:)
                 (setq currentMiner[scoreBias:] high:)
             )
          )
      )
   )
   (setq currentMiner[dropNoResult:] false)
   (setq currentMiner[factoryList:] (makeStructure multivariateRegression:   true
                                                   geneticProgramming:       true
                                                   ruleInduction:            true
                                                   ))
  
   ;;
   ;; Add all filters that are not _Auto
   (setq result (stringToVector (dataMineLib.getTableChildTab filterLambda: #void) #\tab))
   (setq limit (length result))
   (loop for i from 0 until limit do 
      (begin
         (if (<> (left result[i] 5) {_Auto}) 
            (addProjectObject default: filterLambda: result[i])
         )
         (addMinerObject |John Henry|: filterLambda: result[i])
      )
   )
   (setq currentProject.miners[|John Henry|:] currentMiner)
   (setq currentDirectory[projectName] currentProject)
   (saveProjectDirectory)      
   true) ;; end createDefault


   










;;**EXPORTKEY**:readFileRecord
(defun readFileRecord(name start len) 
;; *******************************************************************
;; summary:  Perform a read of a record in the specified text file.
;; Parms:    This procedure accepts three arguments.
;;           name:      The name of the file to open.
;;           start:     The record starting position.
;;           len:       The length of the record to read.
;; Return:   The byte vector containing the record contents.
;; *******************************************************************
   vars:(fileID self (type 0))
   (setq fileID (fileOpen name 0 type))
   (fileSeek fileID start 1)
   (setq self (fileRead fileID len))
   (fileClose fileID 1)
   self) ;; end readFileRecord
   





























;;**EXPORTKEY**:readTextFile
(defun readTextFile(name) 
;; *******************************************************************
;; summary:  Perform a complete read of the specified text file.
;; Parms:    This procedure accepts one argument.
;;           name:      The name of the file to open.
;; Return:   The byte vector containing the complete text file contents.
;; *******************************************************************
   vars:(fileID self (type 0))
   (setq fileID (fileOpen name 0 type))
   (setq self (fileRead fileID))
   (fileClose fileID 1)
   self) ;; end readTextFile
   





























;;**EXPORTKEY**:regress
(defun regress(x y)
;; *******************************************************************
;; name:     regress
;; 
;; summary:  Returns a vector containing the coefficients resulting
;;           from a linear regression on two vectors of equal length. 
;;           If x and y are vectors of equal length,
;;           then (regression x y) is: #(a  b  e).
;;           where a + bx = y represents the least squares best fit
;;           extracted from a comparison of the two vectors. The term
;;           e is the error ä sqr(y - (a + bx)).
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N.
;; Return:   v:       The vector #(a b e)
;; *******************************************************************
    vars:(m n v vt xmean ymean numerator denominator)
    (setq v (new Vector: 3))
    (setq n (length x))
    (setq m (length y))
    (if (<> n m)
        (begin 
           (ringBell)
           (writeln "regress: vectors not the same length")
           (writeln "(length x)=" n)
           (writeln "(length y)=" m)
           (writeln "x=" x)
           (writeln "y=" y)
           (setq n (min m n))))
    (begin
       (setq xmean (avg x))
       (setq ymean (avg y))
       (setq numerator (vectorDotProduct (setq vt (vectorSub x xmean)) y)) 
       (setq denominator (vectorDotProduct vt vt))
       (if (= denominator 0)
           (setq v[1] 0)
           (setq v[1] (/ numerator denominator)))
       (setq v[0] (- ymean (* v[1] xmean)))
       (setq vt (vectorSub (vectorAdd (vectorProduct x v[1]) v[0]) y)) 
       (setq v[2] (vectorDotProduct vt vt)))
    v)





























;;**EXPORTKEY**:regressLambda
(defun regressLambda(minFields maxFields measureName)
;; *******************************************************************
;;  Summary: Builds a History Screening function by using multivariate
;;           regression. Several candidate numeric fields are selected.     
;;           A multiple linear regression is performed, and a strategy, 
;;           using the computed coefficients, is output. 
;; Args:     minFields    The minimum number of fields to regress on.
;;           maxFields    The maximum number of fields to regress on.
;;           measureName  The measure criteria name to regress on.
;; Return:   aSpec         Screen specification text, or #void.
;; *******************************************************************
   pvars:(fieldNameList            ;; The Structure of field names and regression coefficients
          fieldSymbolList          ;; The Structure of field symbols
          numFields                ;; The number of fields to regress on
          measureLambda             ;; The algorithm for measuring each record 
          ;; Methods list
          genCoefficients          ;; Generate valid History Database regression coefficients
          genFieldName             ;; Generate a valid History Database field name
          genRegressExpress        ;; Generate a valid History Database regression expression
          ) ;; end of persistent variables
   vars:(fieldIndex theSpec name)
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> genFieldName #void) (goto Continue:))
   ;; Initialize the inline child Lambdas.
   (defun genCoefficients()
      vars:(lenHistory historyIndex historyTable 
            fieldIndex name i 
            matrix matrixIndex coefVector
            record algorithm yvalue
            ) ;; end of temporary variables
      ;; Select a history time slice at random for regression.
      (setq lenHistory (length historyLambda))
      (setq historyIndex (integer (random lenHistory)))
      (setq historyTable historyLambda[historyIndex 1])
      ;; Select a history time slice at random for regression.
      (setq lenHistory historyTable.rowCount)
      (setq matrix (^new Vector: object: 0))
      (setq matrixIndex 0)
      (loop for historyIndex from 0 until lenHistory do
          (setq record historyTable[historyIndex])
          (if (<> (setq yvalue (measureLambda record)) #void)
              (begin
                 (setq matrix[matrixIndex] (^new Vector: number: (add1 numFields)))
                 (setq matrix[matrixIndex][numFields] yvalue)
                 (loop for fieldIndex from 0 until numFields do
                     (setq matrix[matrixIndex][fieldIndex] record[fieldSymbolList[fieldIndex]])
                     ) ;; end of field loop
                 (++ matrixIndex)
                 )); end if
          ) ;; end of row loop
      ;; Compute the vector of regression coefficients.
      (setq coefVector (multivariableRegress matrix))
      (objectToStructure fieldNameList coefVector)
      true) ;; end of genCoefficients
   (defun genFieldName()
      vars:(fieldType name i unfairFields (ncount 0))
      (setq fieldType |isNumber|:)
      (setq unfairFields measureCriteriaLambda.Pv.unfairFields)
      (while (= name #void) do
          (if (> (++ ncount) 100) (error "genFieldName"))
          (setq i (integer (random (length _usableFields))))
          ;; Do not use fields which are in the current unfair fields Dictionary.
          (if (and (or (= unfairFields #void) 
                       (= unfairFields[_usableFields[i 0]] #void)) 
                   (= fieldType _usableFields[i 1])) 
              (setq name _usableFields[i 0])
              ) ;; end of unfair if
          ) ;; end while
      ;; Accept the field name and add the vertical bars.
      name) ;; end of genFieldName
   (defun genRegressExpress()
      vars:(sexp sop sname termIndex)
      ;; Add none or more math operators.
      (loop for termIndex from 0 until numFields do
         (if (= sexp #void) (setq sexp "") (setq sexp (append sexp "+" #\tab)))
         (setq sexp (append sexp "(" #\tab fieldNameList[termIndex 0] #\tab "*" #\tab fieldNameList[termIndex 1] #\tab ")" #\tab))
         ) ;; end of loop
      ;; Add the top command and the trailing cutoff percent.
      (setq sexp (append "top" #\tab sexp "20%"))
      sexp) ;; end of genRegressExpress
   ;; End initialization of the regression Lambda
   Continue::
   ;; Determine the number of regression fields
   ;; to place in this History screening strategy.
   (setq measureLambda (measureCriteriaLambda.getCriteria measureName))
   (setq measureLambda (regressBuilder measureLambda))
   (setq numFields (max minFields (integer (random (addi maxFields 1)))))
   (setq fieldNameList (new Structure:))
   (setq fieldSymbolList (new Vector: numFields))
   (resize fieldNameList numFields)
   (loop for fieldIndex from 0 until numFields do
       (setq name (genFieldName))
       (setq fieldSymbolList[fieldIndex] name)
       (setq name (string name))
       (if (<> (left name 1) "|") 
           (setq name (append "|" name "|")))
       (setq fieldNameList[fieldIndex 0] name)
       ) ;; end loop
   ;; Compute the proper regression coefficients.
   (genCoefficients)
   ;; Create the top command using the fields and regression coefficients.
   (setq theSpec (genRegressExpress))
   ;; Return the generated regression strategy.
   theSpec) ;; end regressLambda






















;;**EXPORTKEY**:regressBuilder
(defun regressBuilder(aSpec)
;; *******************************************************************
;; Summary:  Builds a History reasurement regression function from the
;;           text specification passed as an argument. The text 
;;           specification is identical to those measurement specifications
;;           sent to the measureBuilder Lambda.
;; Args:     aSpec      Measurement specification text.
;; Return:   proc       aHistory Measurement Lambda.   
;; *******************************************************************
   pvars:(filterFields            ;; Dictionary of valid field names
          unfairFields            ;; Dictionary of unfair field names
          fixupLambda              ;; Lambda for expression transformation
          syntaxLambda             ;; Lambda for expression syntax validation
          rightCommand            ;; The right most statistic command
          ;; Methods list 
          assertRules             ;; Method for defining fix up rules
          buildBody               ;; Method to build the SmartLisp expression body
          buildName               ;; Method for recognizing a History field name
          cmdAll                  ;; Method for parsing the "all" command
          cmdOmit                 ;; Method for parsing the "omit" command
          cmdSort                 ;; Method for parsing the "bottom" and "top" commands
          errorStop               ;; Method to manage error conditions
          replaceRules            ;; Method for recursive criteria replacement
          ) ;; end of persistent variables
   vars:(proc aCmd i n cmds scmd theSpec setCount filterProc)
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> errorStop #void) (goto Continue:))
   ;; Initialize the inline child Lambdas.
   (defun assertRules()
       ;; We only need to run this once.
       (if (isLambda fixupLambda) (return true))
       ;; Define the expression fix up rules.
       (rulesLib)
       (setq fixupLambda (new rulesLib))
       ;; Make sure there are no duplication errors
       (if (= fixupLambda.rulesDic rulesLib.rulesDic) (error "dupRules"))
       ;; Operator name, function name, and number recognition rules
       (fixupLambda.assert $OP:(lambda(x) 
                                 vars:(s (d #{and and or or min min max max + + - - * * / / ** expt < < <= <= = = <> <> >= >= > >})) 
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $REL:(lambda(x) vars:((d #{< < <= <= = = <> <> >= >= > >})) (if (isMember x d) d[x])))
       (fixupLambda.assert $FN1:(lambda(x) 
                                 vars:(s (d #{sin sin cos cos tan tan log log exp exp sqrt sqrt})) 
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $FN2:(lambda(x) 
                                 vars:(s (d #{min min max max}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $NUM:(lambda(x) (if (isNumber x) x)))
       ;; Infix to Prefix notation production rules
       (fixupLambda.assert $PFN:(lambda(a fn x y b) vars:(p) (setq p (list fn x y)) (if (<> b #void) (setq p (append (list p) b))) (if (<> a #void) (setq p (append a p))) p))
       (fixupLambda.assert '($a* $X <$FN=$OP> $Y $b*) '(<$PFN> $a $FN $X $Y $b))
       ;; Symbol referecing production rules
       (fixupLambda.assert $SYM:(lambda(x) (if (= (type x) Symbol:) (makeQuotedSymbol x))))
       (fixupLambda.assert $STR:(lambda(x) (if (isString x) (makeQuotedSymbol x))))
       (fixupLambda.assert '(ref $X <$Y=$SYM>) '(ref $X $Y))
       (fixupLambda.assert '(ref $X <$Y=$STR>) '(ref $X $Y))
       ;; Constant folding production rules
       (fixupLambda.assert $DIV:(lambda(x) (error "measureBuild" (append "measureBuilder divide by zero on: " (string x true)))))
       (fixupLambda.assert $FOLD1:(lambda(op x) vars:(f) (setq f (getGlobalValue (symbol op))) (f x)))
       (fixupLambda.assert $FOLD2:(lambda(op x y) vars:(f) (setq f (getGlobalValue (symbol op))) (f x y)))
       (fixupLambda.assert '(<$Y=$OP> <$X=$NUM> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '(<$Y=$FN1> <$X=$NUM>) '(<$FOLD1> $Y $X))
       (fixupLambda.assert '(<$Y=$FN2> <$X=$NUM> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '(** $X $Y) '(expt $X $Y))
       ;; Algebraic expression reduction rules
       (fixupLambda.assert '(/ $X $X) 1)
       (fixupLambda.assert '(+ $X 0) '$X)
       (fixupLambda.assert '(- $X 0) '$X)
       (fixupLambda.assert '(* $X 0) 0)
       (fixupLambda.assert '(/ 0 0) 0)
       (fixupLambda.assert '(/ $X 0) '(<$DIV> (/ $X 0)))
       (fixupLambda.assert '(expt $X 0) 1)
       (fixupLambda.assert '(+ 0 $X) '$X)
       (fixupLambda.assert '(* 0 $X) 0)
       (fixupLambda.assert '(/ 0 $X) 0)
       (fixupLambda.assert '(expt 0 $X) 0)
       (fixupLambda.assert '(* $X 1) '$X)
       (fixupLambda.assert '(/ $X 1) '$X)
       (fixupLambda.assert '(expt $X 1) '$X)
       (fixupLambda.assert '(* 1 $X) '$X)
       (fixupLambda.assert '(expt 1 $X) 1)
       ;; One based indices reduction rules
       (fixupLambda.assert '(ref (ref x $X) <$Y=$NUM>) '(ref (ref x $X) (sub1 $Y)))
       ;; Excess parentheses reduction rules
       (fixupLambda.assert '(($X*)) '$X)
       ;; Define the expression syntax validation rules.
       (setq syntaxLambda (new rulesLib))
       ;; Make sure there are no duplication errors
       (if (= syntaxLambda.rulesDic fixupLambda.rulesDic) (error "dupRules"))
       ;; Prefix notation recognition rules
       (syntaxLambda.assert '(ref $X $Z) false)
       (syntaxLambda.assert '(<$Y=$FN1> <$X=$NUM>) '(<$FOLD1> $Y $X))
       (syntaxLambda.assert '(<$Y=$OP> $X $Z) false)
       (syntaxLambda.assert '(<$Y=$FN1> $X) false)
       (syntaxLambda.assert '(<$Y=$FN2> $X $Z) false)
       ;; Unrecognized syntax error rules
       (syntaxLambda.assert $ERR:(lambda(x) (error "measureBuild" (append "measureBuilder syntax error on: " (string x true)))))
       (syntaxLambda.assert '$ALL$ '(<$ERR> $ALL$))
       ;; Operator and function name recognition lambda rules
       (syntaxLambda.assert $OP:(lambda(x) 
                                 vars:(s (d #{and and or or min min max max + + - - * * / / expt expt < < <= <= = = <> <> >= >= > >}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FN1:(lambda(x) 
                                 vars:(s (d #{sin sin sub1 sub1 cos cos tan tan log log exp exp sqrt sqrt}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FN2:(lambda(x)
                                 vars:(s (d #{min min max max mapc mapc}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FOLD1:(lambda(op x) vars:(f) (setq f (getGlobalValue (symbol op))) (f x)))
       (syntaxLambda.assert $NUM:(lambda(x) (if (isNumber x) x)))
       true) ;; end assertRules
   (defun buildBody(v p begIndex endIndex)
   ;; Define the build expression body function. This function builds SmartLisp
   ;;  expression bodies.
   ;;  such as (1 {A1} true etc).
       vars:(fieldName s i n)
       (setq s "")
       (loop for i from begIndex until endIndex do
           (setq s (append s " " (buildName v[i] p)))
           ) ;; end loop
       ;; Apply expression transformation rules.
       (fixupLambda.setVerbose false)
       (setq s (lisp s arithmetic:))
       (setq fixupLambda.singlePass false)
       (setq s (fixupLambda.apply s))
       (setq s (syntaxLambda.apply s))
       (setq s (string s true))
       s) ;; end buildBody
   (defun buildName(s p)
   ;; Define the build name function. This function recognizes History
   ;;  field names inclosed in vertical bar | symbols, or SmarLisp constants
   ;;  such as (1 {A1} true etc).
       vars:(fieldName)
       (cond ;; Recognize a History field name inclosed in vertical bar | symbols.
             ;;  Return the construct: p.|Fieldname|
             ((and (isType Symbol: s) (= s[0] #\|))
              (begin
                 (setq fieldName (makeSymbol (mid s 1 (- (length s) 2))))
                 (if (= (member fieldName _validFields) false)
                     (error "buildName"))
                 (setq filterFields[fieldName] _validFields[fieldName])
                 (setq unfairFields[fieldName] true)
                 (append p "." s)))
             ;; Recognize a Legacy field name not inclosed in vertical bar | symbols.
             ;;  Return the construct: p.|Fieldname|
             ((and (isType Symbol: s) (<> (member s _validFields) false))
              (begin
                 (setq fieldName s)
                 (if (= (member fieldName _validFields) false)
                     (error "buildName"))
                 (setq filterFields[fieldName] _validFields[fieldName])
                 (setq unfairFields[fieldName] true)
                 (append p "." fieldName)))
             ;; Recognize all other non-arithmetic operator symbols.
             ;;  Convert the symbol to a string contant.
             ;((and (isType Symbol: s) (= (member s #(+ - * / < > >= <= = <> ref |(| |)| |[| |]|)) false))
             ; (append "{" s "}"))
             ;; Recognize all quoted symbols.
             ;;  Convert the quoted symbol to a string contant.
             ((isType QuotedSymbol: s)
              (append "{" s "}"))
             ;; Assume we have a SmartLisp constant.
             ;;  Return the constant unaltered.
             (else s)
             ) ;; end cond
        ) ;; end buildName
   (defun cmdSet(parms s)
   ;; Define the command parser for the set.
   ;;   for example: "set,X,(,|3 Month Profit|,+,|6 Month Profit|,)"
       vars:(parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (< parmCnt 3)
           (error "regressBuilder" s)) 
       (if (<> parms[0] "set")
           (error "regressBuilder" s)) 
       ;; Build the resulting assignment command.
       (setq outS (append "           (define " parms[1] " " (buildBody parms "_x" 2 parmCnt) ")" _eol "   "))
       outS) ;; end cmdSet
   (defun cmdMeasure(parms s)
   ;; Define the command parser for the (sum avg min max var std).
   ;;   for example: "avg,|3 Month Profit|"
       vars:(parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (< parmCnt 2)
           (error "regressBuilder" s)) 
       (if (isBoolean (member parms[0] #("sum" "avg" "min" "max" "var" "std")))
           (error "memberBuilder" s))
       (setq rightCommand parms[0])
       ;; Build the resulting statistic command.
       (setq outS (append "           (setq _measure " (buildBody parms "_x" 1 parmCnt) ")" _eol "   "))
       outS) ;; end cmdMeasure
   (defun errorStop(err) (writeln "regressBuilder: " err) false)
   (defun pairToVector(v p)
       vars:(i n result)
       (if (= v #void) (setq v (new Vector: 0)))
       (setq n (length p))
       (loop for i from 0 until n do
          (if (isAtom p[i])
              (setq v[(length v)] p[i])
              (begin
                 (setq v[(length v)] |(|:)
                 (setq v (pairToVector v p[i]))
                 (setq v[(length v)] |)|:)
                 )) ;; end if
          ) ;; end loop
       v) ;; end pairToVector
   ;; Run the Lambda.
   Continue::
   (onError errStop)
   (setq filterFields (new Dictionary:))
   (setq unfairFields (new Dictionary:))
   (assertRules)
   ;; Break the command up into its sub-commands.
   (setq cmds (stringToVector aSpec ";"))
   ;; Parse each set command and append to the specification.
   (setq theSpec "")
   (setq setCount (subi (length cmds) 1))
   ;; Parse each set command and append to the specification.
   (loop for i from 0 until setCount do
       (setq cmdVector (pairToVector #void (lisp cmds[i] arithmetic:)))
       (setq scmd (cmdSet cmdVector cmds[i]))
       (setq theSpec (append theSpec scmd))
       ) ;end loop
   ;; Parse the final statistics command and append to the specification.
   (setq cmdVector (pairToVector #void (lisp cmds[i] arithmetic:)))
   (setq scmd (cmdMeasure cmdVector cmds[i]))
   (setq theSpec (append theSpec scmd))
   ;; Convert the measurement specification into a procedure.
   (setq proc (append "(lambda(_x)" _eol "   "))
   (setq proc (append proc "(define _measure #void)" _eol "   "))
   (setq proc (append proc "(defun measure(_x)" _eol "   "))
   (setq proc (append proc "   (onError (lambda(err) (-- _theCount) 0))" _eol))
   (setq proc (append proc theSpec "   ) ;; end measure" _eol "   "))
   (setq filterProc "")
   (loop for i from 0 until (length filterFields) do
      (if (<> filterFields[i 1] #void)
          (begin
             (setq filterProc (append filterProc  
                                _eol 
                                "              (" 
                                filterFields[i 1] 
                                " _x.|" 
                                filterFields[i 0] 
                                "|)")) 
             ))) ;; end loop
   (setq proc (append proc "    (if (and "))
   (setq proc (append proc filterProc ")" _eol "   "))
   (setq proc (append proc "        (begin" _eol "   "))
   (setq proc (append proc "           (measure _x)" _eol "   "))
   (setq proc (append proc "           )) ;; end if" _eol "   "))
   (setq proc (append proc "_measure) ;; end lambda"))
   ;(displayWorkbenchWindow "regressBuilder Output" proc) ; For testing only
   (setq proc (eval proc))
   proc) ;; end of regressBuilder


;;  ***NOTES***:
;; 
;; The selection measurement specification is composed of a series of commands
;; each separated from the other by the semi-colon ; symbol. Some examples
;; might be:
;;
;;     "min,|12 Month Profit|"
;;
;;     "avg,(,|6 Month Profit|,-,|3 Month Profit|,)"
;;
;;     "set,X,|3 Month Profit|;set,Y,(,|6 Month Profit|,-,X,);avg,Y"
;;
;; Notice the semi-colon ; symbol which separates the various commands. Also
;; notice the comma , symbols which separate the command arguments one from
;; another within a command. Each command is composed of a number of arguments.
;; The number of arguments may vary and is dependent upon the command verb
;; which is the first argument in any command, and upon the number and depth
;; of nested parentheses within the expression.
;;
;; Seven command verbs are currently supported. They are as follows:
;;
;;            set  sum  avg  min  max  var  std
;;
;; The final (right most) command must be one of (sum avg min max var std). All
;; of the left most commands must be set commands. Obviously, the set command
;; is only present to allow assignment of temporary variables. Any multiple
;; command measurement can be reexpressed as a single command measurement without
;; temporary variables at the expense of deeply nested parentheses.
;;
;;
;; Each command verb has a distinct meaning and requires somewhat different
;; command arguments. Remember that each command generates code which operates
;; on a stock table by reducing the number of stocks in the table. This is
;; called screening. Commands are evaluated left to right so that the righmost
;; commands always reduces the stock table which results from the leftmost
;; command.
;;
;;
;; **EXAMPLES**
;;
;; For instance we might want to measure the standard deviation of annual profit
;; for a group of selected stocks.
;;
;;     "std,|12 Month Profit|"
;;
;; We might want to measure the worst case quarterly loss for a group of 
;; selected stocks.
;;
;;     "min,|3 Month Profit|"
;;
;;




















;;**EXPORTKEY**:regressProfits
(defun regressProfits(w)
;; *******************************************************************
;; summary:  Returns the M by 3 linear regression coefficient matrix
;;           for each factor giving the best least squares fit to the
;;           variables.
;; Parms:    w:       The N by M+1 matrix representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;; Return:   v:       The M by 5 linear regression coefficient matrix.
;;                    in the form of:    a1 b1 err1 y1 ëy1
;;                                       a2 b2 err2 y2 ëy2
;;                                          ... 
;;                                       aM bM errM y3 ëy3
;;                    where for each x:  ä sqr(a + bx - y) = err
;;                                       and
;;                                       y is the average of the y values
;;                                       for the top 5 (a + bx) predictions.
;;                                       and
;;                                       ëy is the average of the y values
;;                                       for the worst 5 (a + bx) predictions.
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    vars:(w wt v vx vy vr vs i n ymean)
    ;; Extract the least squares error from multiple linear regressions
    (setq n (-1+ (length w[0])))
    (setq v (new Vector: n))
    (setq vy (matrixToVector w n))
    (setq ymean (avg vy))
    (setq vy (vectorSub vy ymean))
    (loop for i from 0 until n do
        (setq vx (matrixToVector w i))
        (setq vr (regress vx vy))
        (setq v[i] (meanProfit vr vx vy)))
    v)
























;;**EXPORTKEY**:ruleInduction
(defun ruleInduction(measureName)
;; *******************************************************************
;;  Summary: This Lambda is in charge of History database rule induction.
;;           Given a measurementCriteriaName (goal Lambda), this Lambda
;;           develops a histogram (in percent clusters) of the actual
;;           scores of each column (including computed columns). The
;;           histogram is used to induce rules via construction and 
;;           development of History Database Screening Strategies which
;;           maximize the specified measurement criteria.
;;           For multiple passes, histograms of pairs, triples, etc. of
;;           columns are constructed.
;; Notes:    Rule induction state is stored in the Blackboard under
;;           the key of "ruleInduction:goalLambdaName" (one for each
;;           goal Lambda being optimized.
;; Args:     measureName    Name of the goal Lambda to optimize against.
;; Return:   true
;; *******************************************************************
   pvars:(;; Persistent variables
          goalLambda                 ;; The measure criteria goal Lambda.
          goalLambdaName             ;; The goal Lambda name.
          ;; Methods list
          errorStop                 ;; Recover from any normal errors.
          ) ;; end persistent variables
   vars:(selection strategy)
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> errorStop #void) (goto Continue:))
   (defun errorStop(err) (writeln "ruleInduction:" err) false)
   ;; Initialization
   Continue::
   (measureCriteriaLambda)
   (selectCriteriaLambda)
   (setq goalLambdaName measureName)
   (setq goalLambda (measureCriteriaLambda.getCriteriaLambda goalLambdaName))
   true) ;; end ruleInduction





















;;**EXPORTKEY**:rulesLib
(defun rulesLib()
;; ********************************************************************
;; summary:  Create, maintain, and apply a Dictionary of IF -> THEN list
;;           substitution rules for transforming a list and all its sub
;;           lists. Anywhere a sub list matches one of the IF rules, the
;;           THEN rule is substituted in place of the original sub list.
;;           This Lambda also supports single or multiple pass substitution
;;           rule application.
;; Parms:    none
;; return:   true
;; ********************************************************************
   pvars:(rulesDic                 ;; Dictionary of morph substitution rules
          singlePass               ;; Switch for single pass morph substitution
          explanation              ;; Contains the explanation after apply
          verbose                  ;; Switch for displaying each explanation step on the console.
          changeCount              ;; Number of rule based substitutions
          (morphFail |*failure*|:) ;; Morph rule failure RHS value
          (maxPasses 200)          ;; Maximum number of passes before issuing error (singlePass = false)
          passCount                ;; Current number of passes already executed by apply
          ;; Methods list 
          apply                    ;; Child to apply all rules in the rules dictionary to morph a list
          assert                   ;; Child to add a single new rule to the rules dictionary 
          bind                     ;; Child to bind dummy variables to values in a sub list 
          doClear                  ;; Child to clear the rules dictionary 
          isMatch                  ;; Child to match a regular expression with a candidate expression 
          len                      ;; Child to return the number of rules in the rules Dictionary 
          listRules                ;; Child for morph to call during rule application
          new                      ;; Child for initializing the newly created list rules Lambda
          ref1                     ;; Child to return the THEN form corresponding to an IF form 
          rulesDictionary          ;; Friend to manage a dictionary of rules
          set1                     ;; Child to add a single new rule to the rules dictionary 
          setFailure               ;; Child to set the rule failure RHS value
          setMaxPasses             ;; Child to set the maximum apply passes limit 
          setSinglePass            ;; Child to set the single pass switch 
          setVerbose               ;; Child to set the verbose switch 
          unassert                 ;; Child to delete a single rule from the rules dictionary 
          ) ;; end of persistent variables
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> new #void) (return true))
   ;; Initialize the inline child Lambdas.
   (defun apply(theList)
       vars:(x)
       (setq x theList)
       (setq passCount 0)
       (setq explanation "")
       Retry::
       (if (> passCount maxPasses) (error "listRule_Pass" "Exceeded maximum number of apply rules."))
       (setq changeCount 0)
       (setq x (morph (list x) listRules morphFail))
       (if (isPair x) (setq x (car x)))
       (if (and (> changeCount 0) (= singlePass false)(isPair x)) (goto Retry:))
       (setq explanation (append explanation "Final result: " (string theList true) " ==> "  (string x true)))
       (if (= verbose true)
           (writeln "Final result: " (string theList true) " ==> "  (string x true)))
       x) ;; end of apply
   (defun assert(ifForm thenForm) (setq rulesDic[ifForm] thenForm))
   (defun bind(mask dic)
   ;; ********************************************************************
   ;; summary:  Bind the specified mask expression (mask) with a Dictionary
   ;;           (dic) containing wild card variables ($Xn) which are bound 
   ;;           to values.
   ;; Parms:    mask:    The mask expression with dummy variables.
   ;;           dic:     The Dictionary of bound variables.
   ;; return:   mask:    The new expression with bound dummy variables replaced.
   ;; ********************************************************************
      vars:(f i n s result (apSW false))
      ;; If the mask is a quoted pair, then set the append switch.
      (if (= (type mask) QuotedPair:)
          (begin
             (setq mask (eval mask))
             (setq apSW true)
             )) ;; end if
      ;; Bind the mask with the Dictionary values.
      (cond ;; If the mask is a List whose first element is a lambda wild
            ;; card variable (ie <$x>), then recursively bind each 
            ;; remaining element of the List, and return the result of 
            ;; applying the wild card lambda value to the final bound List.
            ((and (isPair mask) 
                  (isSymbol mask[0]) 
                  (= (left mask[0] 2) "<$")
                  (= (right mask[0] 1) ">"))
             (begin
                (setq n (length mask))
                (loop for i from 1 until n do (setq mask[i] (bind mask[i] dic)))
                (setq n (length mask[0]))
                (setq s (symbol (mid mask[0] 1 (subi n 2))))
                (setq f rulesDic[s])
                (setq result (^apply f (cdr mask))))) 
            ;; If the mask is a List, then recursively bind each 
            ;; element of the List, and return the final bound List.
            ((isPair mask)
             (setq n (length mask))
             (loop for i from 0 until n do (setq mask[i] (bind mask[i] dic)))
             (setq result mask)) 
            ;; If the mask is a wild card symbol, then return the
            ;; value of the wildcard symbol in the bindings Dictionary.
            ((and (isSymbol mask) (= mask[0] #\$)) 
             (setq result dic[mask]))
            ;; If the mask is anything else, then return the mask as is.
            (else (setq result mask))
            ) ;; end cond
      ;; If the append switch is on, apply append to the result.
      (if (= apSW true) (setq result (^apply append result)))
      result) ;; end of bind
   (defun doClear() (new))
   (defun isMatch(reg exp dic)
   ;; ********************************************************************
   ;; summary:  Match the specified regular expression (reg) with a candidate
   ;;           expression (exp). The regular expression contains dummy
   ;;           variables ($Xn) which are bound to values in the candidate
   ;;           expression using the Dictionary (dic). The act of matching
   ;;           an expression produces a Dictionary with the correct bindings.
   ;; Parms:    reg:     The regular expression with unbound variables ($Xn).
   ;;           exp:     The natural expression to match with the regular.
   ;;           dic:     The Dictionary of bound variables.
   ;; return:   bool:    True if match, or false if no match.
   ;; ********************************************************************
      vars:(i n s pos leftName rightName
            regLeftHand regRightHand wildcd
            expLeftHand expRightHand tmpDic
            recognizeProc recognizeResult nreg 
            ) ;; end of temporary variables
      ;; Any error results in a no match!
      ;(onError (lambda(err) false))
      ;; Attempt to match the regular expression with the input expression.
      (cond ;; If both expressions are Pairs and the first term in the regular
            ;; expression is a named wild card ending with an asterisk $name*,
            ;; then the wild card is matched to the left hand remainder of the
            ;; input expression, and we try to match the right hand portions of the
            ;; regular expression with any right hand portion of the input expression. 
            ((and (isPair reg) (isPair exp) 
                  (isSymbol reg[0]) 
                  (= (left reg[0] 1) "$") 
                  (= (right reg[0] 1) "*")) 
             (begin
                ;; Isolate the wild card name.
                (setq wildcd (mid reg[0] 0 (sub1 (length reg[0]))))
                (setq regRightHand (cdr reg))
                (setq expRightHand exp)
                (setq expLeftHand #void)
                ;; Match immediately, if the wild card terminates the regular expression.
                (if (= regRightHand #void)
                    (begin
                       (setq dic[wildcd] exp)  
                       (return true)
                       )) ;; end if
                ;; Try to match with any right hand portion of the input expression.
                (while (isPair expRightHand)
                    (setq tmpDic (copy dic))
                    (if (isMatch regRightHand expRightHand tmpDic)
                        (if (and (<> tmpDic[wildcd] #void) (compareNE tmpDic[wildcd] expLeftHand))
                            (return false)
                            (begin (setq tmpDic[wildcd] expLeftHand) (objectToDictionary dic tmpDic) (return true))
                            ) ;; end then
                        (begin
                           (setq nreg (car expRightHand))
                           (setq expRightHand (cdr expRightHand))
                           (if (isPair expLeftHand) 
                               (setq expLeftHand (append expLeftHand (list nreg)))
                               (if (= expLeftHand #void) 
                                   (setq expLeftHand (list nreg))
                                   (setq expLeftHand (append expLeftHand (list nreg))))
                               )) ;; end if
                        ) ;; end if
                    ) ;; end while
                false))
            ;; If both expressions are Pairs and the second term in the regular
            ;; expression is a named wild card ending with an asterisk $name*,
            ;; And the input expression has length one, 
            ;; then the wild card is matched to the left hand remainder of the
            ;; input expression, and we try to match the right hand portions of the
            ;; regular expression with any right hand portion of the input expression. 
            ((and (isPair reg) (isPair exp) 
                  (= (length reg) 2) 
                  (isSymbol reg[1]) 
                  (= (left reg[1] 1) "$") 
                  (= (right reg[1] 1) "*") 
                  (= (length exp) 1)) 
             (begin
                (setq tmpDic (copy dic))
                (setq wildcd (mid reg[1] 0 (sub1 (length reg[1]))))
                (if (<> tmpDic[wildcd] #void) (return false))
                (if (isMatch (car reg) (car exp) tmpDic)
                    (begin
                       (objectToDictionary dic tmpDic)
                       (return true))
                    (return false))
                    )) ;; end empty wild card case
            ;; If both expressions are Pairs, try to match their cars and cdrs.
            ((and (isPair reg) (isPair exp))
             (begin
                (setq tmpDic (copy dic))
                (if (and (isMatch (car reg) (car exp) tmpDic) (isMatch (cdr reg) (cdr exp) tmpDic))
                    (begin
                       (objectToDictionary dic tmpDic)
                       (return true))
                    (return false))
                    )) ;; end both pairs case
            ;; If the regular expression is a wild card rule <$name=$rule>,
            ;; and the rule (retrieved from the rules dictionary) returns true,
            ;; then match and bind the wild card except in the case of a
            ;; previous binding with the same wild card which is not equal 
            ;; to this attempted binding.
            ((and (isSymbol reg) (= (left reg 2) "<$"))
             (begin
                (setq nreg (mid reg 1 (subi (length reg) 2)))  
                (cond
                   ((isBoolean (setq pos (find "=$" (string nreg)))) false)
                   ((not (isLambda (setq recognizeProc rulesDic[(setq rightName (mid nreg (addi pos 1) 10000))]))) false)
                   ((= (setq recognizeResult (recognizeProc exp)) false) false)
                   ((= dic[(setq leftName (left nreg pos))] #void) (setq dic[leftName] recognizeResult) true)
                   ((isEqual dic[leftName] recognizeResult) true)
                   (else false))))
            ;; If the regular expression is a wild card symbol (begins with $),
            ;; then match and bind the wild card except in the case of a
            ;; previous binding with the same wild card which is not equal 
            ;; to this attempted binding.
            ((and (isSymbol reg) (= (left reg 1) "$"))
             (cond ((= dic[reg] #void) (setq dic[reg] exp) true)
                   ((isEqual dic[reg] exp) true)
                   (else false)))
            ;; If both expressions are equal, then this is a match!
            ((= reg exp) 
             true)
            ;; Everything else is a no match!
            (else false)
            ) ;; end cond
      ) ;; end of isMatch
   (defun len() (length rulesDic))
   (defun listRules(sexp)
   ;; ********************************************************************
   ;; summary:  If the head of the specified sub list (sexp) matches a rule
   ;;           (ifForm) in the rules Dictionary, then apply the substitution 
   ;;           rule (thenForm) to the sub list passed by the morph procedure.
   ;; ********************************************************************
      vars:(i n dic cpy ret)
      ;(onError (lambda(err) err))      
      (setq n (length rulesDic))
      (loop for i from 0 until n do
          (setq dic (makeDictionary))
          (if (isMatch rulesDic[i 0] sexp dic)
              (begin
                 (++ changeCount)
                 (setq cpy (copy rulesDic[i 1]))
                 (setq ret (bind cpy dic))
                 (setq explanation (append explanation 
                                           "Rule [" i "] replacing: " 
                                           (string sexp true) " ==> "  
                                           (string ret true) _eol))
                 (if (= verbose true) 
                     (writeln "Rule [" i "] replacing: " (string sexp true) " ==> "  (string ret true)))
                 (return ret)
              )) ;; end if
          ) ;; end of loop
      morphFail) ;; end of listRules
   (defun new()
      (setq rulesDic (^new rulesDictionary))
      (setq singlePass true)
      (setq verbose false)) ;; end of new
   (defun ref1(ifForm) 
        (if (isSymbol ifForm) (return (myself)[Pv:][ifForm])) ;; Supports Lambda polymorphism.
        (if (isNumber ifForm)
            (return (spair rulesDic[ifForm 0] rulesDic[ifForm 1]))
            rulesDic[ifForm])) ;; end of ref1
   (defun set1(ifForm thenForm) 
        (if (= ifForm singlePass:) (return (setq singlePass thenForm))) ;; Supports Lambda polymorphism.
        (if (= ifForm verbose:) (return (setq verbose thenForm))) ;; Supports Lambda polymorphism.
        (assert ifForm thenForm)) ;; end of set1
   (defun setFailure(RHSvalue) (setq morphFail RHSvalue))
   (defun setMaxPasses(limit) (setq maxPasses limit))
   (defun setSinglePass(swt) (setq singlePass swt))
   (defun setVerbose(swt) (setq verbose swt))
   (defun unassert(ifForm) (setq rulesDic[ifForm] #void))
   ;; Initialize the Lambda and assign a new rules Dictionary.
   (new)) ;; end of rulesLib


;;  ***NOTES***:
;; 
;; This Lambda provides a rule based capability for transforming
;; an old list into a new list.
;;
;; This Lambda accepts and saves a set of transformation rules in
;; its internal rules dictionary. Each rule is entered, by the
;; programmer, in two parts -- an IF form followed by a THEN form.
;; An example might be:
;; 
;;     (rulesLib.assert '(x + y) '(addi x y))
;;
;; We may then apply the above rules against a list as follows: 
;;
;;     (rulesLib.apply '(x + y))   ==>  '(addi x y)
;;
;; This Lambda supports wild card variables to make rule definitions
;; more flexible for the programmer. An example of wild card rule
;; variables is as follows:
;;
;;     (rulesLib.assert '($X + $Y) '(addi $X $Y))
;;
;; We may then apply the above rules against a list as follows: 
;;
;;     (rulesLib.apply '(m + 10))   ==>  '(addi m 10)
;;
;; The rules and wild card variables operate on sub lists as well
;; as the whole list as follows: 
;;
;;     (rulesLib.apply '((m + 10) + 20))   ==>  '(addi (addi m 10) 20)
;;
;; This Lambda supports named lambda rule definitions which allow
;; more flexible actions to be taken by the programmer during the
;; recognition phase and also during the production phase. Some 
;; examples of named lambda rule definitions is as follows:
;;
;;     (rulesLib.assert $FOLD:(lambda(op x y) vars:(f) (setq f (getGlobalValue (symbol op))) (f x y)))
;;     (rulesLib.assert $NUM:(lambda(x) (if (isNumber x) x)))
;;     (rulesLib.assert $OP:(lambda(x) vars:((d #{+ addi - subi * muli / divi})) (if (isMember x d) d[x])))
;;     (rulesLib.assert '(<$X=$NUM> <$Y=$OP> <$Z=$NUM>) '(<$FOLD> $Y $X $Z))
;;     (rulesLib.assert '($X <$Y=$OP> $Z) '($Y $X $Z))
;;
;; We may then apply the above rules against a list as follows: 
;;
;;     (rulesLib.apply '(m + 10))   ==>  '(addi m 10)
;;     (rulesLib.apply '(5 + 10))   ==>  15
;;
;; Double quoting a right hand production rule causes the append
;; function to be applied to the result as follows:
;;
;;     (rulesLib.assert '($X + $Y) ''((+) $X $Y))
;;     (rulesLib.apply '((5 6 7) + (10 20)))   ==>  '(+ 5 6 7 10 20)
;;
;; This Lambda supports multiple rule definitions up to the limits
;; of available memory. Rules asserted first have precedence over 
;; rules asserted later as follows:
;;
;;     (rulesLib.assert $OP:(lambda(x) vars:((d #{+ addi - subi * muli / divi})) (if (isMemEqv x d) d[x])))
;;     (rulesLib.assert '($X <$Y=$OP> $Z) '($Y $X $Z))
;;     (rulesLib.assert '(addi $X $X) '(muli $X 2))
;;     (rulesLib.assert '(addi $X 0) '$X)
;;
;; We may then apply the multiple rules against a list as follows: 
;;
;;     (rulesLib.apply '(m + m))   ==>  '(addi m m)
;;
;; This Lambda supports multiple passes, during rule application,
;; as follows:
;;
;;     (setq rulesLib.singlePass false)
;;     (rulesLib.apply '(m + m))   ==>  '(muli m 2)
;;     (rulesLib.apply '(m + 0))   ==>  m
;;
;; This Lambda supports asterisk wild card rules anywhere within lists,
;; as follows:
;;
;;     (rulesLib.assert '(min ($X*)) '(<$FN> min $X))
;;     (rulesLib.assert '($X* min)   '(<$FN> min $X))
;;     (rulesLib.assert $FN:(lambda(fn x) (append (list fn) x)))
;;     (rulesLib.apply  '(min (2 3)))  ==>  '(min 2 3)
;;     (rulesLib.apply  '((2 3) min))  ==>  '(min 2 3)
;;
;;









































;;**EXPORTKEY**:rulesLib:rulesDictionary
(defriend rulesLib:rulesDictionary()
;; ********************************************************************
;; summary:  Create, maintain, and apply a Dictionary of IF -> THEN
;;           substitution rules and wild card lambda rules. The wild
;;           card lambda rules are kept separate from the production
;;           rules. The Lambda is designed to behave similar to a 
;;           Dictionary.
;; Parms:    none  
;; return:   true
;; ********************************************************************
   pvars:(leftRULES                ;; Vector of left hand production rules
          rightRULES               ;; Vector of right hand production rules
          lambdaRULES              ;; Dictionary of lambda rules
          ;; Methods list 
          len                      ;; Method to return the number of rules in the rules Dictionary 
          new                      ;; Method to create a new rules Dictionary 
          ref1                     ;; Method to return the THEN form corresponding to an IF form 
          ref2                     ;; Method to return the rules contained in the rules dictionary 
          refCount                 ;; Method to return the number of production rules 
          set1                     ;; Method to add a single new rule to the rules dictionary 
         ) ;; end of persistent variables
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> new #void) (return true))
   ;; Initialize the inline child Lambdas.
   (defun len() (length leftRULES))
   (defun new()
      (setq leftRULES (^new Vector: 0))
      (setq rightRULES (^new Vector: 0))
      (setq lambdaRULES (^new Directory:))
      true) ;; end of new
   (defun ref1(key)
      vars:(i n)
      ;; Are we retrieving wild card lambda rule?
      (if (and (or (isString key) (isSymbol key)) (= key[0] #\$))
          (return lambdaRULES[key]))
      ;; We retrieving right hand production rules.
      (setq n (length leftRULES))
      (loop for i from 0 until n do
          (if (isEqual key leftRULES[i])
              (return rightRULES[i]))
          ) ;; end loop
      #void) ;; end of ref1
   (defun ref2(index1 index2)
      vars:(n)
      (setq n (length leftRULES))
      ;; Are we retrieving a wild card lambda rule?
      (if (>= index1 n)
          (return lambdaRULES[(subi index1 n) index2]))
      ;; Are we retrieving left hand production rules.
      (if (= index2 0)
          (return leftRULES[index1]))
      ;; Are we retrieving right hand production rules.
      (if (= index2 1)
          (return rightRULES[index1]))
      (error "arglist")) ;; end of ref2
   (defun refCount() (length leftRULES))
   (defun set1(key newValue)
      vars:(i n)
      ;; Are we asserting a wild card lambda rule?
      (if (and (isSymbol key) (= key[0] #\$) (<> key $ALL$:))
          (begin
             (if (<> newValue #void)  
                 (return (setq lambdaRULES[key] newValue))) 
             ;; We are deleting a wild card lambda rule.
             (setq i (member key lambdaRULES))
             (if (isNumber i)
                 (delete lambdaRULES i))
             (return #void))) ;; end deleting lambda rule
      ;; We are asserting a production rule.
      (setq n (length leftRULES))
      (setq i (member key leftRULES))
      (if (isNumber i)
          (if (= newValue #void)
              (begin
                 (delete leftRULES i)
                 (delete rightRULES i)
                 (return #void)) ;; end delete production rule
              (begin
                 (setq rightRULES[i] newValue)
                 (return newValue))) ;; end replace production rule
          ) ;; end if
      ;; Do not assert new rules with #void right hand productions.
      (if (= newValue #void) (error "rightHand" (append "rulesDictionary found right hand #void: " key " ==> " newValue)))
      ;; Assert new production rule
      (setq leftRULES[n] key)
      (setq rightRULES[n] newValue)
      newValue) ;; end of set1
   ;; Initialize the Lambda and clear the rules Dictionary.
   (new)) ;; end of rulesDictionary
;;
;; *NOTES*:
;;
;; Rules must be in one of the following forms:
;;
;;      IFFORM                 THENFORM                  DESCRIPTION
;;
;;      $symbol                ..an Lambda..              Lambda wild card rule
;;      $ALL$                  ..anything..              Final production rule
;;      ..other..              ..anything..              Production rule






















;;**EXPORTKEY**:rulesDictionary
(defun rulesDictionary()
;; ********************************************************************
;; summary:  Create, maintain, and apply a Dictionary of IF -> THEN
;;           substitution rules and wild card lambda rules. The wild
;;           card lambda rules are kept separate from the production
;;           rules. The Lambda is designed to behave similar to a 
;;           Dictionary.
;; Parms:    none  
;; return:   true
;; ********************************************************************
   pvars:(leftRULES                ;; Vector of left hand production rules
          rightRULES               ;; Vector of right hand production rules
          lambdaRULES              ;; Dictionary of lambda rules
          ;; Methods list 
          len                      ;; Method to return the number of rules in the rules Dictionary 
          new                      ;; Method to create a new rules Dictionary 
          ref1                     ;; Method to return the THEN form corresponding to an IF form 
          ref2                     ;; Method to return the rules contained in the rules dictionary 
          refCount                 ;; Method to return the number of production rules 
          set1                     ;; Method to add a single new rule to the rules dictionary 
         ) ;; end of persistent variables
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> new #void) (return true))
   ;; Initialize the inline child Lambdas.
   (defun len() (length leftRULES))
   (defun new()
      (setq leftRULES (^new Vector: 0))
      (setq rightRULES (^new Vector: 0))
      (setq lambdaRULES (^new Directory:))
      true) ;; end of new
   (defun ref1(key)
      vars:(i n)
      ;; Are we retrieving wild card lambda rule?
      (if (and (or (isString key) (isSymbol key)) (= key[0] #\$))
          (return lambdaRULES[key]))
      ;; We retrieving right hand production rules.
      (setq n (length leftRULES))
      (loop for i from 0 until n do
          (if (isEqual key leftRULES[i])
              (return rightRULES[i]))
          ) ;; end loop
      #void) ;; end of ref1
   (defun ref2(index1 index2)
      vars:(n)
      (setq n (length leftRULES))
      ;; Are we retrieving a wild card lambda rule?
      (if (>= index1 n)
          (return lambdaRULES[(subi index1 n) index2]))
      ;; Are we retrieving left hand production rules.
      (if (= index2 0)
          (return leftRULES[index1]))
      ;; Are we retrieving right hand production rules.
      (if (= index2 1)
          (return rightRULES[index1]))
      (error "arglist")) ;; end of ref2
   (defun refCount() (length leftRULES))
   (defun set1(key newValue)
      vars:(i n)
      ;; Are we asserting a wild card lambda rule?
      (if (and (isSymbol key) (= key[0] #\$) (<> key $ALL$:))
          (begin
             (if (<> newValue #void)  
                 (return (setq lambdaRULES[key] newValue))) 
             ;; We are deleting a wild card lambda rule.
             (setq i (member key lambdaRULES))
             (if (isNumber i)
                 (delete lambdaRULES i))
             (return #void))) ;; end deleting lambda rule
      ;; We are asserting a production rule.
      (setq n (length leftRULES))
      (setq i (member key leftRULES))
      (if (isNumber i)
          (if (= newValue #void)
              (begin
                 (delete leftRULES i)
                 (delete rightRULES i)
                 (return #void)) ;; end delete production rule
              (begin
                 (setq rightRULES[i] newValue)
                 (return newValue))) ;; end replace production rule
          ) ;; end if
      ;; Do not assert new rules with #void right hand productions.
      (if (= newValue #void) (error "rightHand" (append "rulesDictionary found right hand #void: " key " ==> " newValue)))
      ;; Assert new production rule
      (setq leftRULES[n] key)
      (setq rightRULES[n] newValue)
      newValue) ;; end of set1
   ;; Initialize the Lambda and clear the rules Dictionary.
   (new)) ;; end of rulesDictionary

;;
;; *NOTES*:
;;
;; Rules must be in one of the following forms:
;;
;;      IFFORM                 THENFORM                  DESCRIPTION
;;
;;      $symbol                ..an Lambda..              Lambda wild card rule
;;      $ALL$                  ..anything..              Final production rule
;;      ..other..              ..anything..              Production rule



















;;**EXPORTKEY**:saveObjectFile
(defun saveObjectFile(name type self) 
;; *******************************************************************
;; summary:  Perform a single save of the specified object file.
;; Parms:    This procedure accepts three arguments
;;           name:      The name of the file to create.
;;           type:      The type of the file to create:
;;                      1 = a Spreadsheet object file.
;;                      2 = a Smarttable object file.
;;                      3 = a Workspace object file.
;;                      4 = a binary object file.
;;           self:      The object to be saved in the file.
;; *******************************************************************
   vars:(fileID)
   (setq fileID (fileOpen name 1 type))
   (saveObject fileID self)
   (fileClose fileID 1)
   self)






























;;**EXPORTKEY**:screenLambda
(defun screenLambda(...)
;; *******************************************************************
;;  Summary: Run the various History Database Screening Strategies. 
;;           Each strategy has its own Lambda, and new history database 
;;           screen strategies can be automatically generated.
;; Args:     none
;; Return:   true 
;; *******************************************************************
   pvars:((lastScreen "top,|HHIncome|,>=,$100000,30%"))
   vars:(selection strategy)
   ;; Do we wish to perform automated backtesting? If so, we use the
   ;; blackboard to store the results of back testing for many
   ;; different history database screening strategies. 
   (setq selection (select 
                       "Screening Lambda" 
                       "Please select a command" 
                       #("Automated Criteria Backtesting" 
                         "Manual Criteria Addition"
                         "Automated Criteria Development"
                         ) ;; end of command list 
                       0))
   (if (isBoolean selection)
       (return false))
   (if (= selection 0)
       (return (screenBacktest)))
   (if (= selection 1)
       (begin
          (setq strategy (input "Enter new criteria" "all"))
          (if (isBoolean strategy)
              (return false)
              (screenAddStrategy strategy))
          (return true)
       )) ;; end if
   (if (= selection 2)
       (return (msgbox "Not implemented yet!")))
   true)
























;;**EXPORTKEY**:screenBacktest
(defun screenBacktest()
;; *******************************************************************
;;  Summary: Perform an automated back testing search of the various 
;;           History Screening Strategies. Each strategy has a unique
;;           specification in the language defined by the screenBuilder
;;           procedure.
;; Note:     We use the blackboard to store the results of backtesting
;;           for many different history database screening strategies.
;; Args:     none
;; Return:   true 
;; *******************************************************************
   pvars:(passString)
   vars:(selection x y i n rowIndex
         screenTotals key strategy 
         algorithm thisDate table
         earliestDate earliestIndex
         detailTable detailTableKey
         userDisplayKey
         historyLength historyIndex
         measureTable memoryFree
        ) ;; end of temporary variables
   ;; The results for all history screening strategies back tested so far
   ;; are stored in the blackboard under the key "screenLambda". This
   ;; should be a Smarttable of the totals type (see screenResultTables).
   (onError (lambda(err) (writeln "screenBacktest: " err) false))
   (setq passString (append "[" segmentLambda.passIndex " of " segmentLambda.passMax "] "))
   Restart::
   ;; Make sure that we free as much memory as possible.
   (historyViewer.doClear) 
   (setq screenTotals (screenLoadStrategies))
   ;; Get the last date available in the History database.
   ;; Note:  The last date is used to determine whether or not
   ;;        we need to have further backtesting on a strategy.
   (setq n (sub1 (length historyLambda)))
   (if (< n 0) (return true))
   (setq lastDate historyLambda[n 0])
   ;; Search for any strategies which require more back testing, and
   ;; get the number of History time slices available for back testing. 
   ;; Note: Return if all strategies are fully back tested.
   ;;       If earliest date is #void, make it the first date in 
   ;;       the History database.
   (setq earliestDate lastDate)
   (loop for rowIndex from 0 until screenTotals.rowCount do
       (setq earliestDate (min earliestDate screenTotals[|Last Date|: rowIndex]))
       ) ;; end of loop
   (if (>= earliestDate lastDate) (return true))
   (if (= earliestDate 0)
       (begin
          (setq earliestDate historyLambda[0 0])
          (setq earliestIndex 0)
          ) ;; end then
       (begin
          (setq earliestDate (date earliestDate))
          (setq earliestIndex (addi historyLambda[position: earliestDate] 1))
          (setq earliestDate historyLambda[earliestIndex 0])
          )) ;; end if
   (setq historyLength (subi (length historyLambda) 1))
   ;; Back test all strategies for each time slice. Read the earliest
   ;; history time slice, and then test all required strategies.
   (setq measureTable _Blackboard.measureLambda)
   (if (= measureTable #void) (return true))
   (loop for historyIndex from earliestIndex to historyLength do
       ;; Make sure we do not have any memory leaks.
       ;; Empty any error reporting information. Collect garbage,
       ;; and halt in debug, if memory falls below reasonable levels.
       (setq _error #void)
       (gc)
       (setq memoryFree (inspect))
       ;(if (< memoryFree 10000000) (debug traceon:))
       ;; Read the history data for this time slice.
       (setq table #void)
       (setq thisDate historyLambda[historyIndex 0])
       (writeln passString "Loading History time slice for " thisDate)
       (setq table historyLambda[historyIndex 1])
       ;; Search for any strategies which require more back testing.
       (loop for rowIndex from 0 until screenTotals.rowCount do
           (if (> thisDate screenTotals[|Last Date|: rowIndex])
               (begin
                  ;; Make sure we do not have any memory leaks.
                  ;; Empty any error reporting information. Collect garbage,
                  ;; and halt in debug, if memory falls below reasonable levels.
                  (setq _error #void)
                  (gc)
                  (setq memoryFree (inspect))
                  ;(if (< memoryFree 10000000) (debug traceon:))
                  ;(writeln  passString "Free memory is " memoryFree " bytes.")
                  ;; Get the detail results table for this history screening strategy.
                  ;; Note: If none is available, create a new one. 
                  (setq detailTableKey (append "screenLambda:" screenTotals[rowIndex].Specification))
                  (setq userDisplayKey screenTotals[rowIndex].Name)
                  (setq detailTable _Blackboard[detailTableKey])
                  (if (= detailTable #void)
                      (setq detailTable (screenResultTables detail:)))
                  ;; Truncate detail results tables of any past back testing results
                  (setq n (subi detailTable.rowCount 1))
                  (if (> n historyIndex)
                      (deleteRow detailTable (addi historyIndex 1) (subi n historyIndex)))
                  ;; Backtest the specified strategy against the History database.
                  (writeln passString "Back testing " thisDate " for " userDisplayKey)
                  ;; Mark the criteria so it will not be attempted again if an error occurs.                
                  (setq detailTable[Date: historyIndex] thisDate)
                  (setq screenTotals[|Last Date|: rowIndex] thisDate)
                  (setq screenTotals[|Size|: rowIndex] #void)
                  (setq _Blackboard[screenLambda:] screenTotals)
                  ;; Compile the specification into an Lambda and back test.                
                  ;(writeln "Compiling backtest Lambda " thisDate " for " userDisplayKey)
                  (setq algorithm (screenBuilder screenTotals[Specification: rowIndex]))
                  ;(writeln "Running Lambda " thisDate " for " userDisplayKey)
                  (if (isLambda algorithm)
                      (algorithm table detailTable[historyIndex] measureTable)
                      (table.truncate 0)
                      ) ;; end if
                  (setq screenTotals[|Last Date|: rowIndex] thisDate)
                  ;; Save the measurement totals and detail smarttables in the blackboard.
                  ;(writeln "Computing measure totals " thisDate " for " userDisplayKey)
                  (screenMeasureTotals screenTotals detailTable detailTableKey rowIndex)
                  )) ;; end if
           )    ; end strategies loop
       )    ; end history time slices loop
   (setq table #void)
   (setq algorithm #void)
   (writeln passString "Completed Back testing.")
   true)


































;;**EXPORTKEY**:screenBirth
(defun screenBirth(minCmds maxCmds)
;; *******************************************************************
;;  Summary: Builds a History Screening function from scratch. In     
;;           other words, #void is mapped to a history screening 
;;           strategy. 
;; Args:     maxCmds     The maximum number of sub-commands.       
;; Return:   aSpec       Screen specification text, or #void.
;; *******************************************************************
   pvars:(fieldType                ;; The type of the last generated field name
          fieldName                ;; The name of the last generated field name
          ;; Methods list
          allCmd                   ;; Generate an "all" sub-command
          genFieldName             ;; Generate a valid History Database field name
          genMathOp                ;; Generate a valid mathematical operator
          genRelOp                 ;; Generate a valid relational operator
          genSortExpress           ;; Generate a valid History Database sort expression
          sortCmd                  ;; Generate a "bottom" or "top" sub-command
         ) ;; end of persistent variables
   vars:(numCmds aCmd cmds i n
         cmdIndex scmd theSpec
         ) ;; end of temporary variables
   (defun allCmd()
      vars:(scmd sexp n valuesDic)
      (setq fieldType #void)
      (while (or (= fieldType #void) (= fieldType isString:)) do
          (setq sexp (genSortExpress))
          ) ;; end while
      (setq scmd (append "all" #\tab sexp #\tab (genRelOp) #\tab))
      ;; Add trailing comparison value.
      (case fieldType
          ((isNumber isInteger)
           (setq scmd (append scmd (integer (random 200000)))))    
          ((isBoolean)
           (setq scmd (append scmd #(true false)[(integer (random 2))])))    
          ((isDate)
           (setq scmd (append scmd (substitute (string (date (integer (+ (random 35430) 693975)))) "," "."))))   
          (else
           (begin
              (setq valuesDic (_validFieldValues))
              (setq n (length valuesDic[fieldName]))
              (setq i (integer (random n)))
              (setq scmd (append scmd "{" valuesDic[fieldName][i 1] "}"))))
         ) ;; end case    
      scmd) ;; end of allCmd
   (defun genFieldName(theType)
      vars:(name i)
      (setq fieldType theType)
      (while (= name #void) do
          (setq i (integer (random (length _usableFields))))
          ;; Do not use fields which are in the current unfair fields Dictionary.
          (if (= measureCriteriaLambda.Pv.unfairFields[_usableFields[i 0]] #void)
              (begin
                 (if (and (<> fieldType #void) (= fieldType _usableFields[i 1])) 
                     (setq name _usableFields[i 0]))
                 (if (= fieldType #void)
                     (begin 
                        (setq name _usableFields[i 0])
                        (setq fieldType _usableFields[i 1])
                        )) ;; end of if
              )) ;; end of unfair if
          ) ;; end while
      ;; Accept the field name and add the vertical bars.
      (setq fieldName name)
      (setq name (string name))
      (if (<> (left name 1) "|") 
          (setq name (append "|" name "|")))
      name) ;; end of genFieldName
   (defun genMathOp() #(+ - / * ** min max)[(integer (random 7))])  
   (defun genRelOp() #(< <= = <> > >=)[(integer (random 6))])  
   (defun genSortExpress()
      vars:(sexp sop sname maxTerms termIndex)
      (setq fieldType #void)
      (setq maxTerms (integer (random 5)))
      (setq sexp (genFieldName fieldType))
      ;; Return if not a numeric field.
      (if (and (<> fieldType isNumber:) (<> fieldType isInteger:)) (return sexp)) 
      ;; Add none or more math operators.
      (loop for termIndex from 1 until maxTerms do
         (setq sexp (append sexp #\tab (genMathOp) #\tab (genFieldName fieldType)))
         ) ;; end of loop
      ;; Add enclosing parentheses for compound expressions
      (if (> maxTerms 1) (setq sexp (append "(" #\tab sexp #\tab ")")))
      sexp) ;; end of genSortExpress
   (defun sortCmd(theCmd)
      vars:(scmd)
      (setq scmd (append theCmd #\tab (genSortExpress)))
      ;; Add trailing cutoff percent.
      (setq scmd (append scmd #\tab (addi (integer (random 90)) 5) "%"))     
      scmd) ;; end of sortCmd
   ;; Determine the number of sub-commands to 
   ;; place in this History screening strategy.
   (setq numCmds (max minCmds (integer (random (addi maxCmds 1)))))
   ;; Break the command up into its sub-commands.
   (setq aCmd #void)
   (loop for cmdIndex from 0 until numCmds do
       (case (integer (random 3))
          (0 (begin
                (setq scmd (allCmd))
                )) ;; end case 0  
          (1 (begin
                (setq scmd (sortCmd "bottom"))
                )) ;; end case 1  
          (2 (begin
                (setq scmd (sortCmd "top"))
                )) ;; end case 2
          ) ;; end of case
       (if (= aCmd #void) 
           (setq aCmd scmd)
           (setq aCmd (append aCmd ";" scmd))
           ) ;; end if
       ) ;; end of loop
   ;; Return the generated command.
   aCmd)






















;;**EXPORTKEY**:screenBuilder
(defun screenBuilder(aSpec)
;; *******************************************************************
;;  Summary: Builds a History Screening function from the text specification   
;;           passed as an argument. The text specification is designed
;;           to allow ease of specification, recognition of similar 
;;           history screening strategies, and generation of history
;;           database screening strategies by genetic algorithms.
;; Note:     screenBuilder refers to building history screening procedures,
;;           not to building graphic user interface screens.
;; Args:     aSpec      Screen squeryBuilderpecification text.
;; Return:   proc       aHistory Screen Lambda, or false (if user cancels).
;; *******************************************************************
   pvars:((debugOn false)         ;; Display generated Lambda on switch
          filterFields            ;; Dictionary of valid field names
          fixupLambda              ;; Lambda for expression transformation
          syntaxLambda             ;; Lambda for expression syntax validation
          ;; Methods list 
          assertRules             ;; Method for defining fix up rules
          buildBody               ;; Method to build the SmartLisp expression body
          buildName               ;; Method for recognizing a History field name
          cmdAll                  ;; Method for parsing the "all" command
          cmdOmit                 ;; Method for parsing the "omit" command
          cmdSort                 ;; Method for parsing the "bottom" and "top" commands
          errorStop               ;; Method for handling formula errors
          mergeRules              ;; Method for merging "all" criteria together
          pairToVector            ;; Method for converting a List into a Vector of terms
          replaceRules            ;; Method for recursive criteria replacement
          ) ;; end of persistent variables
   vars:(proc aCmd i j m n cmds scmd theSpec cmdVector)
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> errorStop #void) (goto Continue:))
   ;; Initialize the inline child Lambdas.
   (defun assertRules()
       ;; We only need to run this once.
       (if (isLambda fixupLambda) (return true))
       ;; Define the expression fix up rules.
       (rulesLib)
       (setq fixupLambda (new rulesLib))
       ;; Make sure there are no duplication errors
       (if (= fixupLambda.rulesDic rulesLib.rulesDic) (error "dupRules"))
       ;; Operator name, function name, and number recognition rules
       (fixupLambda.assert $OP:(lambda(x) 
                                 vars:(s (d #{and and or or min min max max + + - - * * / / ** expt < < <= <= = = <> <> >= >= > >})) 
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $REL:(lambda(x) vars:((d #{< < <= <= = = <> <> >= >= > >})) (if (isMember x d) d[x])))
       (fixupLambda.assert $FN1:(lambda(x) 
                                 vars:(s (d #{not not sin sin cos cos tan tan log log exp exp sqrt sqrt})) 
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $FN2:(lambda(x) 
                                 vars:(s (d #{min min max max}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $NAM:(lambda(x) 
                                 vars:((d #(today))) 
                                 (if (not (isMember x d)) x))
                                 ) ;; end assert
       (fixupLambda.assert $NUM:(lambda(x) (if (isNumber x) x)))
       ;; Infix to Prefix notation production rules
       (fixupLambda.assert $PFN:(lambda(a fn x y b) 
                                   vars:(p)
                                   (setq p (list fn x y)) 
                                   (if (<> b #void) (setq p (append (list p) b))) 
                                   (if (<> a #void) (setq p (append a p))) 
                                   p))
       (fixupLambda.assert '($a* $X <$FN=$OP> $Y $b*) '(<$PFN> $a $FN $X $Y $b))
       ;; Constant folding production rules
       (fixupLambda.assert $DIV:(lambda(x) (error "queryBuildExp" (append "queryBuilder divide by zero on: " (string x true)))))
       (fixupLambda.assert $FOLD1:(lambda(op x) vars:(f) (setq f (getGlobalValue (symbol op))) (f x)))
       (fixupLambda.assert $FOLD2:(lambda(op x y) vars:(f) (setq f (getGlobalValue (symbol op))) (f x y)))
       (fixupLambda.assert '(<$Y=$OP> <$X=$NUM> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '(<$Y=$FN1> <$X=$NUM>) '(<$FOLD1> $Y $X))
       (fixupLambda.assert '(<$Y=$FN2> <$X=$NUM> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '(** $X $Y) '(expt $X $Y))
       ;; Algebraic expression reduction rules
       (fixupLambda.assert '(/ $X $X) 1)
       (fixupLambda.assert '(+ $X 0) '$X)
       (fixupLambda.assert '(- $X 0) '$X)
       (fixupLambda.assert '(* $X 0) 0)
       (fixupLambda.assert '(/ 0 0) 0)
       (fixupLambda.assert '(/ $X 0) '(<$DIV> (/ $X 0)))
       (fixupLambda.assert '(expt $X 0) 1)
       (fixupLambda.assert '(+ 0 $X) '$X)
       (fixupLambda.assert '(* 0 $X) 0)
       (fixupLambda.assert '(/ 0 $X) 0)
       (fixupLambda.assert '(expt 0 $X) 0)
       (fixupLambda.assert '(* $X 1) '$X)
       (fixupLambda.assert '(/ $X 1) '$X)
       (fixupLambda.assert '(expt $X 1) '$X)
       (fixupLambda.assert '(* 1 $X) '$X)
       (fixupLambda.assert '(expt 1 $X) 1)
       ;; _ANY_ empty indices reduction rules
       (fixupLambda.assert '(<$O=$REL> (ref (ref (ref x $X)) $Y) $Z) 
                          '(_ANY_ ($O (ref x $Y) $Z) (ref x $X)))
       ;; One based indices reduction rules
       (fixupLambda.assert '(ref (ref x $X) <$Y=$NUM>) '(ref (ref x $X) (sub1 $Y)))
       ;; Excess parentheses reduction rules
       (fixupLambda.assert '(($X*)) '$X)
       (fixupLambda.assert '(<$X=$NAM>) '$X)
       ;; Define the expression syntax validation rules.
       (setq syntaxLambda (new rulesLib))
       (syntaxLambda.setFailure false)
       ;; Make sure there are no duplication errors
       (if (= syntaxLambda.rulesDic fixupLambda.rulesDic) (error "dupRules"))
       ;; Prefix notation recognition rules
       (syntaxLambda.assert '(ref $X $Z) false)
       (syntaxLambda.assert '(<$Y=$FN1> <$X=$NUM>) '(<$FOLD1> $Y $X))
       (syntaxLambda.assert '(<$Y=$OP> $X $Z) false)
       (syntaxLambda.assert '(<$Y=$FN1> $X) false)
       (syntaxLambda.assert '(<$Y=$FN2> $X $Z) false)
       (syntaxLambda.assert '(today) false)
       ;; _ANY_ macro substitution rules
       (syntaxLambda.assert '(_ANY_ $A $B) 
                           '(mapc (lambda(x) (let ((r r)) (if $A (setq r true)) r)) $B))
       ;; Unrecognized syntax error rules
       (syntaxLambda.assert $ERR:(lambda(x) (error "queryBuildExp" (append "queryBuilder syntax error on: " (string x true)))))
       (syntaxLambda.assert '$ALL$ '(<$ERR> $ALL$))
       ;; Operator and function name recognition lambda rules
       (syntaxLambda.assert $OP:(lambda(x) 
                                 vars:(s (d #{and and or or min min max max + + - - * * / / expt expt < < <= <= = = <> <> >= >= > >}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FN1:(lambda(x) 
                                 vars:(s (d #{isBoolean isBoolean isNumber isNumber isDate isDate isSymbol isSymbol isString isString isDictionary isDictionary not not sin sin sub1 sub1 cos cos tan tan log log exp exp sqrt sqrt}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FN2:(lambda(x)
                                 vars:(s (d #{min min max max mapc mapc}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FOLD1:(lambda(op x) vars:(f) (setq f (getGlobalValue (symbol op))) (f x)))
       (syntaxLambda.assert $NUM:(lambda(x) (if (isNumber x) x)))
       true) ;; end assertRules
   (defun buildBody(v p begIndex endIndex)
   ;; Define the build expression body function. This function builds SmartLisp
   ;;  expression bodies.
   ;;  such as (1 {A1} true etc).
       vars:(fieldName s i n)
       (setq s "")
       (loop for i from begIndex until endIndex do
           (setq s (append s " " (buildName v[i] p)))
           ) ;; end loop
       ;; Apply expression transformation rules.
       (if (= p "x")
           (fixupLambda.setVerbose false)
           (fixupLambda.setVerbose false))
       (setq s (lisp s arithmetic:))
       (setq fixupLambda.singlePass false)
       (setq s (fixupLambda.apply s))
       ;(syntaxLambda.setVerbose true)
       (setq s (syntaxLambda.apply s))
       (setq s (string s true))
       s) ;; end buildBody
   (defun buildName(s p)
   ;; Define the build name function. This function recognizes Legacy
   ;;  field names inclosed in vertical bar | symbols, or SmarLisp constants
   ;;  such as (1 {A1} true etc).
       vars:(fieldName)
       (cond ;; Recognize a Legacy field name inclosed in vertical bar | symbols.
             ;;  Return the construct: p.|Fieldname|
             ((and (isType Symbol: s) (= s[0] #\|))
              (begin
                 (setq fieldName s)
                 (if (= (member fieldName _validFields) false)
                     (error "buildName"))
                 (setq filterFields[fieldName] _validFields[fieldName])
                 (append p "." fieldName)))
             ;; Recognize a Legacy field name not inclosed in vertical bar | symbols.
             ;;  Return the construct: p.|Fieldname|
             ((and (isType Symbol: s) (<> (member s _validFields) false))
              (begin
                 (setq fieldName s)
                 (setq filterFields[fieldName] _validFields[fieldName])
                 (append p "." fieldName)))
             ;; Recognize the today operator symbols.
             ;;  Convert the symbol to a List contant.
             ((= s today:)
              (append "(" s ")"))
             ;; Recognize all other non-arithmetic operator symbols.
             ;;  Convert the symbol to a string contant.
             ((and (isType Symbol: s) (= (member s #(isBoolean isNumber isDate isSymbol isString isDictionary and or not + - * / < > >= <= = <> ref |(| |)| |[| |]|)) false))
              (append "{" s "}"))
             ;; Repackage string constants within braces.
             ;;  Convert the string to a string contant.
             ((and (isString s) (= (member s #(isBoolean isNumber isDate isSymbol isString isDictionary and or not + - * / < > >= <= = <> ref |(| |)| |[| |]|)) false))
              (append "{" s "}"))
             ;; Recognize all quoted symbols.
             ;;  Convert the quoted symbol to a string contant.
             ((isType QuotedSymbol: s)
              (append "{" s "}"))
             ;; Assume we have a SmartLisp constant.
             ;;  Return the constant unaltered.
             (else s)
             ) ;; end cond
        ) ;; end buildName
   (defun cmdAll(parms s)
   ;; Define the "all" command parser.
   ;;   for example: "all = |Timeliness| 1"
       vars:(parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (= parmCnt 1)
           (return "")) 
       (if (< parmCnt 4)
           (error "screenBuilder" s)) 
       (if (<> parms[0] "all")
           (error "screenBuilder" s)) 
       ;; Build the resulting find command.
       (setq outS (append "(ST.find (lambda(x) (onError (lambda(s) false)) "
                          (buildBody parms "x" 1 parmCnt) "))"))
       outS) ;; end cmdAll
   (defun cmdFilter(parms s)
   ;; Define the "all" command parser.
   ;;   for example: "all = |Timeliness| 1"
       vars:(parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (= parmCnt 1)
           (return "")) 
       (if (< parmCnt 4)
           (error "screenBuilder" s)) 
       (if (<> parms[0] "all")
           (error "screenBuilder" s)) 
       ;; Build the resulting find command.
       (setq outS (append "(ST.find (lambda(x) (onError (lambda(s) false)) "
                          (buildBody parms "x" 1 parmCnt) "))"))
       outS) ;; end cmdFilter
   (defun cmdOmit(parms s)
   ;; Define the "all" command parser.
   ;;   for example: "omit |Timeliness| = 5"
       vars:(parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (= parmCnt 1)
           (return "")) 
       (if (< parmCnt 4)
           (error "screenBuilder" s)) 
       (if (<> parms[0] "omit")
           (error "screenBuilder" s)) 
       ;; Build the resulting find command.
       (setq outS (append "(ST.find (lambda(x) (onError (lambda(s) false)) (not "
                          (buildBody parms "x" 1 parmCnt) ")))"))
       outS) ;; end cmdOmit
   (defun cmdSort(parms s)
   ;; Define the "top", "bottom", & "sort" command parsers.
   ;;   for example: "top |Price| 5"
       vars:(parmCnt lastParm outS n ratio sortOp)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       (setq lastParm (subi parmCnt 1))
       ;; Check command for number of arguments.
       (cond 
           ;; Manage simple truncate: "top cutoff"
           ((= parmCnt 2)
            (begin 
               (if (= parms[0] "top")
                   (error "topInvalid")
                   (setq sortOp "<="))
               (cond ((isInteger parms[1])
                      (setq ratio (append "" parms[1])))
                     ((and (isNumber parms[1]) (< parms[1] 1))
                      (setq ratio (append "(integer (* ST.rowCount " parms[1] "))")))
                     ((= parms[1] "all")
                      (setq ratio "ST.rowCount"))
                     (else
                      (error "screenBuilder" parms))
                   ) ;; end cond
               ;; Build the resulting find command.
               (setq outS (append "(ST.truncate " ratio ")"))))
           ;; Manage simple field sort: "top value1 cutoff"
           ((= parmCnt 3)
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">=")
                   (setq sortOp "<="))
               (cond ((isInteger parms[2])
                      (setq ratio (append "" parms[2])))
                     ((and (isNumber parms[2]) (< parms[2] 1))
                      (setq ratio (append "(integer (* ST.rowCount " parms[2] "))")))
                     ((= parms[2] "all")
                      (setq ratio "ST.rowCount"))
                     (else
                      (error "screenBuilder" parms))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(ST.sort (lambda(x y) (onError (lambda(s) false)) (" sortOp
                                  " " (buildName parms[1] "x")
                                  " " (buildName parms[1] "y") ")))" _eol "   "))
               (setq outS (append outS "(ST.truncate " ratio ")"))))
           ;; Manage simple expression sort: "top value1 / value2 cutoff" 
           ;; Note: (for backward compatibility).
           ((and (= parmCnt 5) (isNumber (member parms[2] #("+" "-" "*" "/"))))
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">=")
                   (setq sortOp "<="))
               (cond ((isInteger parms[4])
                      (setq ratio (append "" parms[4])))
                     ((and (isNumber parms[4]) (< parms[4] 1))
                      (setq ratio (append "(integer (* ST.rowCount " parms[4] "))")))
                     ((= parms[4] "all")
                      (setq ratio "ST.rowCount"))
                     (else
                      (error "screenBuilder" parms))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(ST.sort (lambda(x y) (onError (lambda(s) false)) (" sortOp
                                   " (" parms[2] " " (buildName parms[1] "x") " " (buildName parms[3] "x") ")"
                                   " (" parms[2] " " (buildName parms[1] "y") " " (buildName parms[3] "y") ")"
                                   ")))" _eol "   "))
               (setq outS (append outS "(ST.truncate " ratio ")"))))
           ;; Manage complex expression sort: "top / value1 value1 cutoff" 
           (else
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">=")
                   (setq sortOp "<="))
               (cond ((isInteger parms[lastParm])
                      (setq ratio (append "" parms[lastParm])))
                     ((and (isNumber parms[lastParm]) (< parms[lastParm] 1))
                      (setq ratio (append "(integer (* ST.rowCount " parms[lastParm] "))")))
                     ((= parms[lastParm] "all")
                      (setq ratio "ST.rowCount"))
                     (else
                      (error "queryBuilder" parms))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(ST.sort (lambda(x y) (onError (lambda(s) false)) (" sortOp " "
                                   (buildBody parms "x" 1 lastParm) " "
                                   (buildBody parms "y" 1 lastParm)
                                  ")))" _eol "   "))
               (setq outS (append outS "(ST.truncate " ratio ")"))))
           ) ;end cond
       outS) ;; end cmdSort
   (defun errorStop(err) (writeln "screenBuilder: " err) false)
   (defun mergeRules(cmds)      
   ;; Merges consecutive "all" rules together with an "and" conjunction.
   ;; Looks for "all" rules and eliminates them.
       vars:(cmdIndex args newCmds ruleLambda i)
       (loop for cmdIndex from 0 until (length cmds) do
           (cond
              ;; Replace "all" command with nothing,
              ;; because and "all" command is a noop.
              ((= cmds[cmdIndex] "all")
               (begin
                  (setq cmds (delete cmds cmdIndex))
                  (setq cmdIndex (max (- cmdIndex 2) -1))
                  )) ;; end begin
              ;; Replace "remarks" command with nothing,
              ;; because and "remarks" command is a comment.
              ((= (left cmds[cmdIndex] 7) "remarks")
               (begin
                  (setq cmds (delete cmds cmdIndex))
                  (setq cmdIndex (max (- cmdIndex 2) -1))
                  )) ;; end begin
              ;; Replace "omit" command with "all (not )",
              ((= (left cmds[cmdIndex] 4) "omit")
               (begin
                  (setq newCmds (append "all (not ("
                                        (mid cmds[cmdIndex] 4 100000)
                                        "))"))
                  (setq cmds[cmdIndex] newCmds)
                  (setq cmdIndex (max (- cmdIndex 2) -1))
                  )) ;; end begin
              ;; Merge consecutive "all" commands together,
              ;; with a conjunctive "and" operator.
              ((and (= (left cmds[cmdIndex] 3) "all")
                    (< (add1 cmdIndex) (length cmds))
                    (= (left cmds[(add1 cmdIndex)] 3) "all"))
               (begin
                  (setq newCmds (append "all ("
                                        (mid cmds[cmdIndex] 3 100000)
                                        ") and ("
                                        (mid cmds[(add1 cmdIndex)] 3 100000)
                                        ")"))
                  (setq cmds (delete cmds cmdIndex))
                  (setq cmds[cmdIndex] newCmds)
                  (setq cmdIndex (max (sub1 cmdIndex) -1))
                  )) ;; end begin
               ) ;; end cond
           ) ;; end of loop
       cmds) ;; end of mergeRules
   (defun pairToVector(v p)
       vars:(i n result)
       (if (= v #void) (setq v (new Vector: 0)))
       (setq n (length p))
       (loop for i from 0 until n do
          (if (isAtom p[i])
              (setq v[(length v)] p[i])
              (begin
                 (setq v[(length v)] |(|:)
                 (setq v (pairToVector v p[i]))
                 (setq v[(length v)] |)|:)
                 )) ;; end if
          ) ;; end loop
       v) ;; end pairToVector
   (defun replaceRules(cmds)
   ;; Recursively replaces rules with their command strings.
   ;; Looks for "selection" rules in the selectCriteriaLambda.
   ;; Looks for "exclusion" rules in the excludCriteriaLambda.
       vars:(cmdIndex args newCmds ruleLambda i)
       (loop for cmdIndex from 0 until (length cmds) do
           ;; Check for rule command
           (if (= (left cmds[cmdIndex] 8) "criteria")
               (begin
                  (setq args (objectToVector (lisp cmds[cmdIndex] arithmetic:)))
                  ;; Assign the type of rule (selection or exclusion).
                  (cond ((= (length args) 2)
                         (setq ruleLambda selectCriteriaLambda))
                        ((and (= (length args) 3) (= args[2] selection:))
                         (setq ruleLambda selectCriteriaLambda))
                        ((and (= (length args) 3) (= args[2] exclusion:))
                         (setq ruleLambda excludCriteriaLambda))
                        (else 
                         (error "ruleType"))
                        ) ;; end cond
                  ;; Retrieve the rule string and 
                  ;; covert to a vector of commands
                  (if (and (= (left args[1] 1) "{") (= (right args[1] 1) "}")) 
                      (setq args[1] (mid args[1] 1 (subi (length args[1]) 2))))
                  (setq newCmds (ruleLambda.Pv.getCriteria args[1]))
                  (setq newCmds (stringToVector newCmds ";"))
                  ;; Replace substitute rules within the original 
                  ;; vector of command strings.
                  (setq cmds[cmdIndex] newCmds[0])
                  (loop for i from 1 until (length newCmds) do
                      (setq cmds (vectorInsert cmds (addi cmdIndex i) newCmds[i]))
                      ) ;; end loop
                  ;; Make sure we retest the first substituted rule.
                  (-- cmdIndex)
                  )) ;; end if
           ) ;; end of loop
       cmds) ;; end of replaceRules
   ;; Run the Lambda.
   Continue::
   (onError errorStop)
   (setq filterFields (new Dictionary:))
   (assertRules)
   ;; Break the command up into its sub-commands.
   (setq cmds (stringToVector aSpec ";"))
   ;; Perform recursive rule substitution.
   (setq cmds (replaceRules cmds)) 
   ;; Establish the filter names array.
   (loop for i from 0 until (length cmds) do
       ;; Establish the filter names for each command.
       (setq cmdVector (pairToVector #void (lisp cmds[i] arithmetic:)))
       (loop for j from 0 until (length cmdVector) do
           (buildName cmdVector[j] "x")
           ) ;; end j loop
       ) ;end i loop
   ;; Add the filter names array as additional restrictions (if "nocheck" command not present).
   (if (and (> (length cmds) 0) (<> (trim cmds[0]) "nocheck"))
       (begin
          (loop for i from 0 until (length filterFields) do
             (if (and (<> filterFields[i 1] #void) (<> filterFields[i 1] isDictionary:))
                 (begin
                    (setq aCmd (append "all (" filterFields[i 1] " |" filterFields[i 0] "|)"))
                    (setq cmds (vectorInsert cmds 0 aCmd))  
                    ))) ;; end loop
          )) ; end if
   ;; Perform rule merging.
   (setq cmds (mergeRules cmds))
   ;; Parse each sub command and append to the specification.
   (setq theSpec "")
   (loop for i from 0 until (length cmds) do
       ;; Call the proper command parser (based on the command word).
       (setq cmdVector (pairToVector #void (lisp cmds[i] arithmetic:)))
       (cond 
           ((= cmdVector[0] "all") (setq scmd (cmdAll cmdVector cmds[i]))) 
           ((= cmdVector[0] "check") (setq scmd ""))
           ((= cmdVector[0] "nocheck") (setq scmd ""))
           ((= cmdVector[0] "bottom") (setq scmd (cmdSort cmdVector cmds[i])))
           ((= cmdVector[0] "omit") (setq scmd (cmdOmit cmdVector cmds[i]))) 
           ((= cmdVector[0] "top") (setq scmd (cmdSort cmdVector cmds[i])))
           (else (error "screenBuilder" cmds[i]))
           ) ; end cond
       (setq theSpec (append theSpec scmd _eol "   "))
       ) ;end loop
   ;; Convert the screen specification into a procedure.
   (setq proc (append "(lambda(ST ...)" _eol "   "))
   (setq proc (append proc "vars:(mean detailRow measureTable)" _eol "   "))
   (setq proc (append proc "pvars:(STable)" _eol "   "))
   (setq proc (append proc "(defun errorStop(err) (if (isLambda STable) (STable.truncate 0)) (setq STable #void) (writeln "algorithm:" err) false)" _eol "   "))
   (setq proc (append proc "(onError errorStop)" _eol "   "))
   (setq proc (append proc "(setq STable ST)" _eol "   "))
   (setq proc (append proc "(if (= (argCount) 3)" _eol "   "))
   (setq proc (append proc "    (begin" _eol "   "))
   (setq proc (append proc "       (setq detailRow (argFetch 1))" _eol "   "))
   (setq proc (append proc "       (setq measureTable (argFetch 2))" _eol "   "))
   (setq proc (append proc "       (ST.findAll))" _eol "   "))
   (setq proc (append proc "    (begin" _eol "   "))
   (setq proc (append proc "       (setq mean true)))" _eol "   "))
   (if (<> theSpec "")
       (setq proc (append proc theSpec)))
   (setq proc (append proc "(if (= (argCount) 3)" _eol "   "))
   (setq proc (append proc "    (setq mean (screenProfits ST detailRow measureTable)))" _eol "   "))
   (setq proc (append proc "(setq STable #void)" _eol "   "))
   (setq proc (append proc "mean)"))
   (if (= debugOn true) (writeln proc)) ;; Use for testing only
   (setq proc (eval proc))
   (if (not (isLambda proc)) (error "screenBuilder"))
   proc) ;; end of screenBuilder



















;;**EXPORTKEY**:screenBuilderNotes
;;  ***NOTES***: screenBuilder
;;
;; The table screen specification is composed of a series of commands
;; each separated from the other by the semi-colon ; symbol. Some examples
;; would be:
;;
;;     "all Timeliness = 1;top Sales 3"
;;
;;     "all Timeliness < 3;top Sales 10%"
;;
;; Notice the semi-colon ; symbol which separates the two commands. Also
;; notice the whitespace which separate the command arguments one from
;; another within a command. Each command is composed of a number of arguments.
;; The number of arguments may vary and is dependent upon the command verb
;; which is the first argument in any command.
;;
;; Five command verbs are currently supported. They are as follows:
;;
;;            all  top  bottom  omit  criteria
;;
;; A special field validity test command to omit filter code for each field
;; (referenced in the following query) may be specified as follows:
;;
;;            check
;;
;; Each command verb has a distinct meaning and requires somewhat different
;; command arguments. Remember that each command generates code which operates
;; on a database table by reducing the number of rows in the table. This is
;; called screening. Commands are evaluated left to right so that the righmost
;; commands always reduces the database table which results from the leftmost
;; command.
;;
;; For instance we might want to screen of all rows with a financial rating
;; of A1 and whose yield is in the top third. The following commands perform
;; such a screen.
;;
;;     "all |Financial Rating| = {A1};top |Dividend PS| / Price 30%"
;;
;; **ALL**
;;
;; The "all" command finds all table rows for which the expression is true.
;; There must be at least three arguments and there may be N arguments.
;;
;;   Arguments:
;;
;;     value1      Must be a valid scalar expression.
;;     relational  Must be a relational operator: = <> <= >= < >
;;     valueN      Must be a valid scalar expression.
;;
;;   Example One:
;;
;;     "all |Financial Rating| = {A1}"
;;
;;     The following code is generated
;;
;;     (table.select (lambda(x) (= x.|Financial Rating| {A1}))) 
;;
;;   Example Two:
;;
;;     "all (|Price| / |Earnings PS|) < 20"
;;
;;     The following code is generated
;;
;;     (table.select (lambda(x) (< (/ x.|Price| x.|Earnings PS|) 2)))
;;
;;   Example Three:
;;
;;     "all DepAccounts[].MonthEndBalance >= $10000"
;;
;;     The following code is generated
;;
;;     (table.select (lambda(x) (mapc (lambda(x) (let ((r r)) (if (>= x.MonthEndBalance $10000) (setq r true)) r)) x.DepAccounts)))
;;
;; **BOTTOM**
;;
;; The "bottom" command sorts the table in ascending order on the variable listed
;; in its arguments. After sorting, the bottom N items are selected. A fixed number
;; of items may be found or a ratio may be found. There must be at least two 
;; arguments and there may be N arguments.
;;
;;   Arguments:
;;
;;    (option One)
;;     value1      Must be a valid scalar expression.
;;     cutoff      Must be a numeric constant, a percent such as 30%, or all
;;
;;    (option Two)
;;     value1      Must be a valid scalar expression.
;;     function    Must be a valid infix operator name: + - / * exp sqr etc.
;;     valueN      Must be a valid scalar expression.
;;     cutoff      Must be a numeric constant, a percent such as 30%, or all
;;
;;   Example One:
;;
;;     "bottom Price 5"
;;
;;     The following sort/find code is generated
;;
;;     (table.sort (lambda(x y) (< x.Price y.Price))) 
;;     (table.truncate 5) 
;;
;;   Example Two: (for backward compatibility)
;;
;;     "bottom Price / |Earnings PS| 5"
;;
;;     The following sort/find code is generated
;;
;;     (table.sort (lambda(x y) (< (/ x.Price x.|Earnings PS|) (/ y.Price y.|Earnings PS|)))) 
;;     (table.truncate 5) 
;;
;;   Example Three:
;;
;;     "bottom (Price / |Earnings PS|) 5"
;;
;;     The following sort/find code is generated
;;
;;     (table.sort (lambda(x y) (< (/ x.Price x.|Earnings PS|) (/ y.Price y.|Earnings PS|)))) 
;;     (table.truncate 5) 
;;
;; **TOP**
;;
;; The "top" command sorts the table in descending order on the variable listed
;; in its arguments. It is identical to "bottom" except in sort ordering.
;;
;; **OMIT**
;;
;; The "omit" command tells the system to exclude the rows specified from
;; the current table. It is identical to "all" except it excludes instead
;; includes.
;;
;; **CRITERIA**
;;
;; The "criteria" command substitutes previously saved and named criteria,
;; specified in the named rule, in place of the criteria command. There must 
;; be at least one argument, and there may be as many as two arguments. 
;;
;;   Arguments:
;;
;;     name        Must be the valid name of a Selection or Exclusion Strategy.
;;     type        Must be either selection or exclusion. If omitted, selection
;;                 is assumed.
;;
;;   Example One:
;;
;;     Selection rule (Strong_Stocks) = "all |Financial Rating| = {A1}"
;;     Exculsion rule (Weak_Stocks)   = "omit |Technical Rank| = 5"
;;     Selection rule (Weak_Stocks)   = "all |Technical Rank| = 5"
;;
;;     "criteria,Weak_Stocks"
;;
;;     The following string results after rule substitution
;;
;;     "all |Technical Rank| = 5"
;;
;;     The following find code is generated
;;
;;     (table.select (lambda(x) (= x.|Technical Rank| 5))) 
;;





















;;**EXPORTKEY**:screenLoadStrategies
(defun screenLoadStrategies()
;; *******************************************************************
;; Summary:  Load the specified History Screening Strategy to the screen
;;           Lambdas list of strategies to be backtested. Each strategy 
;;           has a unique specification in the language defined by the 
;;           screenBuilder procedure.
;; Note:     We use the blackboard to store the results of backtesting
;;           for many different history database screening strategies.
;; Args:     none
;; Return:   screenTotals   The new screen Lambdas list of strategies
;;                          for backtesting. 
;; *******************************************************************
   vars:(x y i n screenTotals key strategy)
   ;; Obtain the specification of the strategy to add.
   (if (>= (argCount) 1)
       (setq strategy (argFetch 0))
       (setq strategy #void))
   ;; The results for all history screening strategies back tested so far
   ;; are stored in the blackboard under the key "screenLambda". This
   ;; should be a Smarttable of the totals type (see screenResultTables). 
   (setq screenTotals _Blackboard.screenLambda)
   ;; If the blackboard is empty, then we must create a totals smarttable
   ;; and enter an initial history screening strategy for back testing. 
   (if (= screenTotals #void)
       (setq screenTotals (_defaultScreenStrategies))
       (setq _Blackboard.screenLambda screenTotals)
       ) ;; end if
   screenTotals)






















;;**EXPORTKEY**:screenMeasureScoreVector
(defun screenMeasureScoreVector(detailTable measureColKey)
;; *******************************************************************
;;  Summary: Create a measurement statistics vector showing the results
;;           of back testing of a specified History Screening Strategy 
;;           from the detailed history screening strategies results 
;;           table. Each strategy has a unique specification in the
;;           language defined by the screenBuilder Lambda.         
;; Note:     We use the blackboard to store the results of backtesting
;;           for many different history database screening strategies.
;; Note:     See also segmentLambda.delimitedScores, which performs a
;;           similar function for measurement score display on the client.
;; Args:     detailTable       The detail measurement table for a strategy.
;;           detailColKey      The detail measurement table column key.
;; Return:   scores            The vector of measurement score statistics.
;; *******************************************************************
   pvars:((percentOfHistories .85)  ;; The mandatory percent of Histories which must have counts.
          (scoreCount 7)            ;; The number of separate statistics for each score vector.
         ) ;; end of persistent variables
   vars:(scores tableRows actualCount rowIndex
         ndata navg nsum nmin nmax nstd nrar nnum ncnt
         ) ;; end Temporary variables
   ;; If there are no detailed measurements, then return void.
   (if (= detailTable #void) (return #void))
   ;; First create a nil measurement statistics vector,
   ;; with default statistics temporary total fields.
   (setq scores (new Vector: scoreCount 0))
   (setq actualRows 0)
   (setq navg 0)
   (setq nsum 0)
   (setq nmin 999999999999999)
   (setq nmax -999999999999999)
   (setq nstd 0)
   (setq nrar 0)
   (setq nnum 0)
   ;; Compute the scores for the specified selection
   ;; criteria and the specified measurement goal.
   (setq tableRows detailTable.rowCount)
   (if (= tableRows 0) (return #void))
   (loop for rowIndex from 0 until tableRows do
       (setq ndata detailTable[measureColKey rowIndex])
       (setq ncnt detailTable[Size: rowIndex])
       (+= nnum ncnt)
       (if (and (<> ndata #void) (<> ncnt 0))
           (begin
              (++ actualCount)
              (+= navg ndata)
              (+= nstd (* ndata ndata))
              (setq nmin (min nmin ndata))
              (setq nmax (max nmax ndata))
              )) ;; end if
       ) ;; end loop
   ;; Compute statistics only if there are history rows present and the number 
   ;; of actual score rows exceeds the mandatory percentage of total rows.
   (if (and (> tableRows 0) (> actualCount 0) (>= (/ actualCount tableRows) percentOfHistories))
       (begin
          (/= navg actualCount)
          (setq nsum (* navg nnum))
          (/= nstd actualCount)
          (-= nstd (* navg navg)) 
          (setq nstd (sqrt nstd))
          (if (<> nstd 0) (setq nrar (/ navg nstd))) 
          (setq scores[0] navg)
          (setq scores[1] nsum)
          (setq scores[2] nmin)
          (setq scores[3] nmax)
          (setq scores[4] nstd)
          (setq scores[5] nrar)
          (setq scores[6] nnum)              
          )
       (setq scores #void)
       ) ;; end if
   ;; Return the measurement statistics vector.
   scores) ;; end screenMeasureScoreVector
































;;**EXPORTKEY**:screenMeasureTotals
(defun screenMeasureTotals(screenTotals detailTable detailTableKey rowIndex)
;; *******************************************************************
;;  Summary: Compute the back testing statistics for the specified History
;;           Screening Strategy row in the history screening strategies 
;;           results table. Each strategy has a unique specification 
;;           in the language defined by the screenBuilder Lambda.
;; Note:     We use the blackboard to store the results of backtesting
;;           for many different history database screening strategies.
;; Args:     screenTotals      The history screen strategies results table
;;                             stored in the blackboard (see screenSearch
;;                             and screenLambda).
;;           detailTable       The detail measurement table for this
;;                             strategy.
;;           detailTableKey    The detail measurement table repository key.
;;           rowIndex          The row index of the totals table.
;; Return:   true 
;; *******************************************************************
   vars:(i n col detailCol)
   ;; Clear the mean value field for each measurement strategy.
   (loop for col from 4 until screenTotals.colCount do
       (setq screenTotals[col rowIndex] #void)
       ) ;; end clear loop
   ;; Compute the mean value for each measurement strategy.
   (loop for col from 4 until screenTotals.colCount do
       (setq detailCol (subi col 3))
       (setq screenTotals[col rowIndex] (screenMeasureScoreVector detailTable detailCol))
       ) ;; end col loop
   ;; Save the totals and detail smarttables in the blackboard.
   (beginTransaction _Blackboard)
   (setq _Blackboard[detailTableKey] detailTable)
   (setq _Blackboard[screenLambda:] screenTotals)
   (commitTransaction _Blackboard)
   true)
































;;**EXPORTKEY**:screenMutate
(defun screenMutate(popStrategy)
;; *******************************************************************
;; Summary:  Builds a History Screening strategy by mutating random    
;;           portions of the parents genomes with the random genomes. 
;; Args:     oneStrategy     History screening strategy number one.
;;           twoStrategy     History screening strategy number two.
;; Return:   babyStrategy    The generated History screening strategy.
;; *******************************************************************
   vars:(popStrategy mutationPoint
         popVector popLen
         babyStrategy
         i j n m
         ) ;; end of temporary variables
   ;; Divide the parent History screening strategy into its genomes.
   (setq popVector (stringToVector popStrategy ";"))
   (setq popLen (length popVector))
   ;; Determine the mutation point for the parent.
   (setq mutationPoint (integer (random popLen)))
   ;; Using the mutation point, create the babys genome.
   (setq popVector[mutationPoint] (screenBirth 1 1))
   ;; Covert the parents genome back into a History screening strategy.
   (setq babyStrategy "")
   (loop for i from 0 until popLen do
       (if (<> babyStrategy "") (setq babyStrategy (append babyStrategy ";")))
       (setq babyStrategy (append babyStrategy popVector[i]))
       ) ;; end append loop   
   ;; Return the generated History screening strategy.
   babyStrategy)






















;;**EXPORTKEY**:screenMutation
(defun screenMutation(aSpec)
;; *******************************************************************
;;  Summary: Builds a History Screening function by mutating the text    
;;           specification passed as an argument. The text specification
;;           is designed to allow ease of specification, recognition 
;;           of similar history screening strategies, and generation 
;;           of history database screening strategies by genetic algorithms.
;; Args:     aSpec      Screen specification text.
;; Return:   proc       aHistory Screen Lambda, or an error.   
;; *******************************************************************
   pvars:(filterFields            ;; Dictionary of valid field names
          fixupLambda              ;; Lambda for expression transformation
          ;; Methods list 
          assertRules             ;; Method for defining fix up rules
          buildBody               ;; Method to build the SmartLisp expression body
          buildName               ;; Method for recognizing a History field name
          cmdAll                  ;; Method for parsing the "all" command
          cmdOmit                 ;; Method for parsing the "omit" command
          cmdSort                 ;; Method for parsing the "bottom" and "top" commands
          replaceRules            ;; Method for recursive criteria replacement
         ) ;; end of persistent variables
   vars:(proc aCmd i n cmds scmd theSpec)
   (defun assertRules()
       ;; We only need to run this once.
       (if (isLambda fixupLambda) (return true))
       ;; Define the expression fix up rules.
       (rulesLib)
       (setq fixupLambda (new rulesLib))
       (fixupLambda)
       ;; Prefix notation rules
       (fixupLambda.assert '(ref x $Z) '(ref x $Z))
       (fixupLambda.assert '(<$Y=$OP> <$X=$NUM> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '(<$Y=$OP> $X $Z) '($Y $X $Z))
       (fixupLambda.assert '(<$Y=$FN1> <$X=$NUM>) '(<$FOLD1> $Y $X))
       (fixupLambda.assert '(<$Y=$FN2> <$X=$NUM> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       ;; Function call notation rules
       (fixupLambda.assert '(<$Y=$FN1> (<$X=$NUM>)) '(<$FOLD1> $Y $X))
       (fixupLambda.assert '(<$Y=$FN1> ($X)) '($Y $X))
       (fixupLambda.assert '(<$Y=$FN1> ($X) $A) '(($Y $X) $A))
       (fixupLambda.assert '(<$Y=$FN1> ($X) $A $B) '(($Y $X) $A $B))
       (fixupLambda.assert '(<$Y=$FN2> (<$X=$NUM> <$Z=$NUM>)) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '(<$Y=$FN2> ($X $Z)) '($Y $X $Z))
       (fixupLambda.assert '(<$Y=$FN2> ($X $Z) $A) '(($Y $X $Z) $A))
       (fixupLambda.assert '(<$Y=$FN2> ($X $Z) $A $B) '(($Y $X $Z) $A $B))
       ;; Infix notation notation rules
       (fixupLambda.assert '(<$X=$NUM> <$Y=$OP> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '($X <$Y=$OP> $Z) '($Y $X $Z))
       ;; Excess parentheses rules
       (fixupLambda.assert '(($X $Y $Z)) '($X $Y $Z))
       (fixupLambda.assert '(($X $Y)) '($X $Y))
       (fixupLambda.assert '(($X)) '($X))
       ;; Unrecognized syntax error rule
       (fixupLambda.assert '$X '(<$ERR> $X))
       ;; Lambda notation rules
       (fixupLambda.assert $ERR:(lambda(x) (error "scnBuildExp" (append "screenBuilder error on: " (string x true)))))
       ;; Constant folding rules
       (fixupLambda.assert $FOLD1:(lambda(op x) vars:(f) (setq f (getGlobalValue (symbol op))) (f x)))
       (fixupLambda.assert $FOLD2:(lambda(op x y) vars:(f) (setq f (getGlobalValue (symbol op))) (f x y)))
       (fixupLambda.assert $NUM:(lambda(x) (if (isNumber x) x)))
       ;; Operator and function name recognition rules
       (fixupLambda.assert $OP:(lambda(x) vars:((d #{and and or or + + - - * * / / ** expt < < <= <= = = <> <> >= >= > >})) (if (isMember x d) d[x])))
       (fixupLambda.assert $FN1:(lambda(x) vars:((d #{sin sin cos cos tan tan log log exp exp sqrt sqrt})) (if (isMember x d) d[x])))
       (fixupLambda.assert $FN2:(lambda(x) vars:((d #{min min max max})) (if (isMember x d) d[x])))
       true) ;; end assertRules
   (defun buildBody(v p begIndex endIndex)
   ;; Define the build expression body function. This function builds SmartLisp
   ;;  expression bodies.
   ;;  such as (1 {A1} true etc).
       vars:(fieldName s i n)
       (setq s "")
       (loop for i from begIndex until endIndex do
           (setq s (append s " " (buildName v[i] p)))
           ) ;; end loop
       ;; Apply expression transformation rules.
       (setq fixupLambda.singlePass true)
       (setq s (fixupLambda.applyRight (lisp s)))
       (setq s (fixupLambda.apply s))
       (setq s (string s true))
       s) ;; end buildBody
   (defun buildName(s p)
   ;; Define the build name function. This function recognizes History
   ;;  field names inclosed in vertical bar | symbols, or SmarLisp constants
   ;;  such as (1 {A1} true etc).
       vars:(fieldName)
       (cond ;; Recognize a History field name inclosed in vertical bar | symbols.
             ;;  Return the construct: p.|Fieldname|
             ((= s[0] #\|)
              (begin
                 (setq fieldName (makeSymbol (mid s 1 (- (length s) 2))))
                 (if (= (member fieldName _validFields) false)
                     (error "buildName"))
                 (setq filterFields[fieldName] _validFields[fieldName])
                 (append p "." s)))
             ;; Assume we have a SmartLisp constant.
             ;;  Return the constant unaltered.
             (else s)
             ) ;; end cond
        ) ;; end buildName
   (defun cmdAll(s)
   ;; Define the "all" command parser.
   ;;   for example: "all,=,|Timeliness|,1"
       vars:(parms parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parms (stringToVector s #\tab))
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (= parmCnt 1)
           (return "")) 
       (if (< parmCnt 4)
           (error "screenBuilder" s)) 
       (if (<> parms[0] "all")
           (error "screenBuilder" s)) 
       (if (and (= parmCnt 4)
                (isBoolean (member parms[2] #("=" "<>" "<=" ">=" "<" ">"))))
           (error "screenBuilder" s))
       ;; Build the resulting find command.
       (setq outS (append "(ST.find (lambda(x) "
                          (buildBody parms "x" 1 parmCnt) "))"))
       outS) ;; end cmdAll
   (defun cmdOmit(s)
   ;; Define the "all" command parser.
   ;;   for example: "omit,=,|Timeliness|,5"
       vars:(parms parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parms (stringToVector s #\tab))
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (= parmCnt 1)
           (return "")) 
       (if (< parmCnt 4)
           (error "screenBuilder" s)) 
       (if (<> parms[0] "omit")
           (error "screenBuilder" s)) 
       (if (and (= parmCnt 4)
                (isBoolean (member parms[2] #("=" "<>" "<=" ">=" "<" ">"))))
           (error "screenBuilder" s))
       ;; Build the resulting find command.
       (setq outS (append "(ST.find (lambda(x) (not "
                          (buildBody parms "x" 1 parmCnt) ")))"))
       outS) ;; end cmdOmit
   (defun cmdSort(s)
   ;; Define the "top" & "bottom" command parsers.
   ;;   for example: "top,|Price|,5"
       vars:(parms parmCnt lastParm outS n ratio sortOp)
       ;; Break the command up into its parameters.
       (setq parms (stringToVector s #\tab))
       (setq parmCnt (length parms))
       (setq lastParm (subi parmCnt 1))
       ;; Check command for number of arguments.
       (cond 
           ;; Manage simple field sort: "top,value1,cutoff"
           ((= parmCnt 3)
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">")
                   (setq sortOp "<"))
               (cond ((= parms[2][0] #\/)
                      (setq ratio (append "(subi (divi ST.rowCount " (integer (mid parms[2] 1 10)) ") 1)")))
                     ((= (right parms[2] 1) "%")
                      (setq ratio (append "(subi (integer (* ST.rowCount " (/ (number parms[2]) 100) ")) 1)")))
                     (else
                      (setq ratio (append "(subi " (integer parms[2]) " 1)")))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(ST.sort (lambda(x y) (" sortOp
                                  " " (buildName parms[1] "x")
                                  " " (buildName parms[1] "y") ")))" _eol "   "))
               (setq outS (append outS "(ST.find 0 " ratio ")"))))
           ;; Manage simple expression sort: "top,value1,/,value2,cutoff" 
           ;; Note: (for backward compatibility).
           ((and (= parmCnt 5) (isNumber (member parms[2] #("+" "-" "*" "/"))))
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">")
                   (setq sortOp "<"))
               (cond ((= parms[4][0] #\/)
                      (setq ratio (append "(subi (divi ST.rowCount " (integer (mid parms[4] 1 10)) ") 1)")))
                     ((= (right parms[4] 1) "%")
                      (setq ratio (append "(subi (integer (* ST.rowCount " (/ (number parms[4]) 100) ")) 1)")))
                     (else
                      (setq ratio (append "(subi " (integer parms[4]) " 1)")))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(ST.sort (lambda(x y) (" sortOp
                                   " (" parms[2] " " (buildName parms[1] "x") " " (buildName parms[3] "x") ")"
                                   " (" parms[2] " " (buildName parms[1] "y") " " (buildName parms[3] "y") ")"
                                   ")))" _eol "   "))
               (setq outS (append outS "(ST.find 0 " ratio ")"))))
           ;; Manage complex expression sort: "top,/,value1,value1,cutoff" 
           (else
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">")
                   (setq sortOp "<"))
               (cond ((= parms[lastParm][0] #\/)
                      (setq ratio (append "(subi (divi ST.rowCount " (integer (mid parms[lastParm] 1 10)) ") 1)")))
                     ((= (right parms[lastParm] 1) "%")
                      (setq ratio (append "(subi (integer (* ST.rowCount " (/ (number parms[lastParm]) 100) ")) 1)")))
                     (else
                      (setq ratio (append "(subi " (integer parms[lastParm]) " 1)")))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(ST.sort (lambda(x y) (" sortOp " "
                                   (buildBody parms "x" 1 lastParm) " "
                                   (buildBody parms "y" 1 lastParm)
                                  ")))" _eol "   "))
               (setq outS (append outS "(ST.find 0 " ratio ")"))))
           ) ;end cond
       outS) ;; end cmdSort
   (defun replaceRules(cmds)
   ;; Recursively replaces rules with their command strings.
   ;; Looks for "selection" rules in the selectCriteriaLambda.
   ;; Looks for "exclusion" rules in the excludCriteriaLambda.
       vars:(cmdIndex args newCmds ruleLambda i)
       (loop for cmdIndex from 0 until (length cmds) do
           ;; Check for rule command
           (if (= (left cmds[cmdIndex] 8) "criteria")
               (begin
                  (setq args (stringToVector cmds[cmdIndex] #\tab))
                  ;; Assign the type of rule (selection or exclusion).
                  (cond ((= (length args) 2)
                         (setq ruleLambda selectCriteriaLambda))
                        ((and (= (length args) 3) (= args[2] "selection"))
                         (setq ruleLambda selectCriteriaLambda))
                        ((and (= (length args) 3) (= args[2] "exclusion"))
                         (setq ruleLambda excludCriteriaLambda))
                        (else 
                         (error "ruleType"))
                        ) ;; end cond
                  ;; Retrieve the rule string and 
                  ;; covert to a vector of commands
                  (if (and (= (left args[1] 1) "{") (= (right args[1] 1) "}")) 
                      (setq args[1] (mid args[1] 1 (subi (length args[1]) 2))))
                  (setq newCmds (ruleLambda.Pv.getCriteria args[1]))
                  (setq newCmds (stringToVector newCmds ";"))
                  ;; Replace substitute rules within the original 
                  ;; vector of command strings.
                  (setq cmds[cmdIndex] newCmds[0])
                  (loop for i from 1 until (length newCmds) do
                      (setq cmds (vectorInsert cmds (addi cmdIndex i) newCmds[i]))
                      ) ;; end loop
                  ;; Make sure we retest the first substituted rule.
                  (-- cmdIndex)
                  )) ;; end if
           ) ;; end of loop
       cmds) ;; end of replaceRules
   ;; Initialize.
   (setq filterFields (new Dictionary:))
   (assertRules)
   ;; Break the command up into its sub-commands.
   (setq cmds (stringToVector aSpec ";"))
   ;; Perform recursive rule substitution.
   (setq cmds (replaceRules cmds)) 
   ;; Parse each sub command and append to the specification.
   (setq theSpec "")
   (loop for i from 0 until (length cmds) do
       ;; Call the proper command parser (based on the command word).
       (if (= cmds[i] "all")
           (setq aCmd "all")
           (setq aCmd (left cmds[i] (find #\tab cmds[i]))))
       (cond 
           ((= aCmd "all") (setq scmd (cmdAll cmds[i]))) 
           ((= aCmd "top") (setq scmd (cmdSort cmds[i])))
           ((= aCmd "bottom") (setq scmd (cmdSort cmds[i])))
           ((= aCmd "omit") (setq scmd (cmdOmit cmds[i]))) 
           (else (error "screenBuilder" cmds[i]))
           ) ; end cond
       (setq theSpec (append theSpec scmd _eol "   "))
       ) ;end loop
   ;; Convert the screen specification into a procedure.
   (setq proc (append "(lambda(ST ...)" _eol "   "))
   (setq proc (append proc "vars:(mean detailRow measureTable)" _eol "   "))
   (setq proc (append proc "(onError (lambda(err) #void))" _eol "   "))
   (setq proc (append proc "(defun filterProc(x) (and "))
   (loop for i from 0 until (length filterFields) do
      (if (<> filterFields[i 1] #void)
          (begin
             (setq proc (append proc  
                                _eol 
                                "                             (" 
                                filterFields[i 1] 
                                " x.|" 
                                filterFields[i 0] 
                                "|)")) 
             ))) ;; end loop
   (setq proc (append proc "))" _eol "   "))
   (setq proc (append proc "(if (= (argCount) 2)" _eol "   "))
   (setq proc (append proc "    (begin" _eol "   "))
   (setq proc (append proc "       (setq detailRow (argFetch 1))" _eol "   "))
   (setq proc (append proc "       (setq measureTable (argFetch 2)))" _eol "   "))
   (setq proc (append proc "    (begin" _eol "   "))
   (setq proc (append proc "       (setq mean true)" _eol "   "))
   (setq proc (append proc "       (ST.findAll)))" _eol "   "))
   (setq proc (append proc "(ST.find filterProc)" _eol "   "))
   (if (<> theSpec "")
       (setq proc (append proc theSpec)))
   (setq proc (append proc "(if (= (argCount) 2)" _eol "   "))
   (setq proc (append proc "    (setq mean (screenProfits ST detailRow measureTable)))" _eol "   "))
   (setq proc (append proc "mean)"))
   (displayWorkbenchWindow "screenBuilder Output" proc) ;; Use for testing only
   (setq proc (eval proc))
   (if (not (isLambda proc)) (error "screenBuilder"))
   proc)






















;;**EXPORTKEY**:screenProfits
(defun screenProfits(ST detailRow measureTable)
;; *******************************************************************
;; summary:  Computes the results of all measurement strategies for
;;           the current selections.
;; Args:     ST            History tableLambda containing the selections.
;;           detailRow     screenLambda detail results row (see screenResultTables).
;;           measureTable  Measurement Strategies table (see screenResultTables).
;; Return:   true   
;; *******************************************************************
   vars:(rowCount measureCount measureIndex
        ) ;; end of temporary variables
   ;;  Find the selection size and the measurement count.
   (setq rowCount ST.rowCount)
   ;;  Do not measure if there are no rows selected.
   (if (= rowCount 0) (return false))
   (setq measureCount measureTable.rowCount)
   (setq detailRow.Size rowCount)
   ;;  For each measurement strategy, measure the results for these selections.
   (loop for measureIndex from 0 until measureCount do
       (setq detailRow[(addi measureIndex 2)] (measureTable[Lambda: measureIndex] ST))
       ) ;; end of measure loop
   true)




























;;**EXPORTKEY**:screenReproduce
(defun screenReproduce(oneStrategy twoStrategy)
;; *******************************************************************
;; Summary:  Builds a History Screening strategy by combining random    
;;           portions of the mothers genomes with the fathers genomes. 
;; Args:     oneStrategy     History screening strategy number one.
;;           twoStrategy     History screening strategy number two.
;; Return:   babyStrategy    The generated History screening strategy.
;; *******************************************************************
   vars:(momStrategy popStrategy
         momVector momLen momCrossOver 
         popVector popLen popCrossOver
         babyVector babyLen babyStrategy
         i j n m
         ) ;; end of temporary variables
   ;; Randomly decide who is the mother and who is the father.
   (if (< (random 1) .5)
       (begin
          (setq momStrategy oneStrategy)
          (setq popStrategy twoStrategy)
          ) ;; end then
       (begin
          (setq momStrategy twoStrategy)
          (setq popStrategy oneStrategy)
          ) ;; end else
       ) ;; end if
   ;; Divide each History screening strategy into its genomes.
   (setq popVector (stringToVector popStrategy ";"))
   (setq popLen (length popVector))
   (setq momVector (stringToVector momStrategy ";"))
   (setq momLen (length momVector))
   ;; Determine the cross over point for both parents.
   (setq momCrossOver (integer (random momLen)))
   (setq popCrossOver (integer (random popLen)))
   ;; Using the cross over points, create the babys genome.
   (setq babyVector (new Vector: 1))
   (loop for i from 0 to popCrossOver do
       (setq babyVector[i] popVector[i])
       ) ;; end father loop
   (setq n (- momLen momCrossOver))
   (loop for j from 0 until n do
       (setq babyVector[(+ i j)] momVector[(+ momCrossOver j)])
       ) ;; end father loop
   ;; Covert the babys genome back into a History screening strategy.
   (setq babyStrategy "")
   (setq babyLen (length babyVector))
   (loop for i from 0 until babyLen do
       (if (<> babyStrategy "") (setq babyStrategy (append babyStrategy ";")))
       (setq babyStrategy (append babyStrategy babyVector[i]))
       ) ;; end append loop   
   ;; Return the generated History screening strategy.
   babyStrategy)






















;;**EXPORTKEY**:screenResultTables
(defun screenResultTables(ttype)  
;; *******************************************************************
;;  Summary: Create the various History Screening Strategies results
;;           Smarttables, and also the History Measurement formula
;;           tables. Each measurement strategy can be uniquely identified
;;           via the simple specification language defined by the
;;           measureBuilder Lambda. Each history screening strategy can be 
;;           uniquely identified via the simple specification language
;;           defined by the screenBuilder Lambda. The screenLambda
;;           stores the results of its searches in the blackboard
;;           repository. There is a totals table which holds the rollup
;;           results totals for all history screening strategies for all
;;           periods, and for each period there is a detail table which
;;           holds the detailed results for all history screening 
;;           strategies for that period alone.
;; Args:     ttype    The type of screenLambda results table to create.
;;                    (totals:   Create a new screenLambda totals table).
;;                    (detail:   Create a new screenLambda detail table).
;;                    (exclude:  Create a new screenLambda exclusion table).
;;                    (measure:  Create a new screenLambda measurement table).
;; Return:            The newly created screenLambda results table. 
;; *******************************************************************
   vars:(measureTable measureIndex measureCount detailTable colIndex)
   (cond 
      ((= ttype prod:)
       ;; Create a prodLambda criteria smarttable.
       (new Smarttable:
          #{|Specification|            |key|        ;; Complete legacyBuilder specification for this strategy.
            |Name|                     |formula|    ;; The name given to this strategy.
            |Notes|                    |formula|    ;; The notes describing this strategy.
           })) ;; end new totals
      ((= ttype totals:)
       ;; Create a screenLambda totals smarttable.
       ;; Note:  A totals results table is created and stored in the 
       ;;        blackboard holding rollup totals for all dates where 
       ;;        multiple history database screening strategies are 
       ;;        back tested.
       (new Smarttable:
          #{|Specification|            |key|        ;; Complete screenBuilder specification for this strategy.
            |Name|                     |formula|    ;; The name given to this strategy.
            |Notes|                    |formula|    ;; The notes describing this strategy.
            |Last Date|                |formula|    ;; Last date of portfolio backtested for this strategy.
            |Size|                     |formula|    ;; Average selection size for this strategy.
           })) ;; end new totals
      ((= ttype detail:)
       ;; Create a screenLambda detail smarttable.
       ;; Note:  A detail results table is created and stored in the blackboard
       ;;        for each history screening strategy to be back tested. Each detail
       ;;        smarttable is stored in the blackboard using a concatenated string
       ;;        key where the string "screenLambda:" is appended with the complete
       ;;        screenBuilder specification for this strategy.
       (begin
           (setq detailTable 
               (new Smarttable:
                  #{|Date|                     |key|        ;; Complete screenBuilder specification for this strategy.
                    |Size|                     |formula|    ;; Selection size for this strategy.
                    }))
           (setq measureTable _Blackboard.measureLambda)
           (if (<> measureTable #void)
               (setq measureCount measureTable.rowCount)
               (setq measureCount 0))
           ;; Add new measurement columns to the detail results table.
           (loop for measureIndex from 0 until measureCount do 
               (setq colIndex detailTable.colCount)
               (addCol detailTable 1 colIndex)
               (setq detailTable.colVector[colIndex].name (symbol measureTable[Name: measureIndex]))
               ) ;; end loop
           detailTable)) ;; end new detail
      ((= ttype exclude:)
       ;; Create a screenLambda exclusion strategy smarttable.
       ;; Note:  An exclusion strategies table is created and stored in the blackboard
       ;;        which stores all history exclusion strategies to be back tested.
       (new Smarttable:
          #{|Specification|            |key|        ;; Complete screenBuilder specification for this strategy.
            |Name|                     |formula|    ;; The name given to this strategy.
            |Notes|                    |formula|    ;; The notes describing this strategy.
           })) ;; end new detail
      ((= ttype measure:)
       ;; Create a screenLambda measurement formula smarttable.
       ;; Note:  A measurement formula table is created and stored in the blackboard
       ;;        which stores all history measurement formula for measuring back testing.
       (new Smarttable:
          #{|Specification|            |key|        ;; Complete measureBuilder specification for this formula.
            |Name|                     |formula|    ;; The name given to this measurement formula.
            |Notes|                    |formula|    ;; The notes describing this measurement formula.
            |Lambda|                    |formula|    ;; The compiled version of the specification from measureBuilder.
            |Unfair|                   |formula|    ;; The vector of unfair fields for this specification from measureBuilder.
           })) ;; end new detail
       ) ;; end cond
   ) ;; end of screenResultsTable



























;;**EXPORTKEY**:securityLambda
(defun securityLambda()
;; *******************************************************************
;; summary:  Manages all security functions for the Predictive Modeler
;; Args:     none
;; Return:   true
;; *******************************************************************
  pvars:(myParent                ;;holds the parent Lambda object reference
         currentUser             ;;currently loaded user record
         ;;methods
         addUser                 ;;adds a user record
         clearUserBase           ;;clears the security database
         createUserStructure     ;;creates the structure used to store user security info
         deleteUser              ;;deletes a user record
         getUserNames            ;;returns list of user names
         getUserSecurity         ;;returns a particular user's record
         getSecurityInfo         ;;returns all user records
         loginUser               ;;sets the user online status  
         updateUser              ;;update the user security record  
         )   ;;end of persistant variables
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistent storage of the original Lambda.
    (if (<> addUser #void) (goto Continue:))
    ;; Initialize the inline child Lambdas.
    (defun addUser(userName userPassword userLevel userScript)
       ;; Create an empty user record
       (createUserStructure)
       ;; Assign values to new user record
       (setq currentUser.userName userName)
       (setq currentUser.userPassword userPassword)
       (setq currentUser.userLevel userLevel)
       (setq currentUser.userScript userScript)
       (setq _SecurityDB[userName] currentUser)
       true) ;; eop addUser
    (defun clearUserBase()
       (clear _SecurityDB)
       (addUser "demo" "demo" 300 "PmTest.sl")) ;; eop clearUserBase
    (defun createUserStructure()
       (setq currentUser (makeStructure userName:     #void 
                                        userPassword: #void 
                                        userLevel:    #void 
                                        userScript:   #void
                                        userOnline:   #void
                         )
       )) ;; eop createUserStructure
    (defun deleteUser(userName) 
       ;;attempt to load user record
       ;;void user in directory and current user 
       (setq _SecurityDB[userName] #void)
       (setq currentUser #void)
       ;; return true
       true) ;; eop deleteUser
    (defun getUserNames() 
       ;; Return the User Names
       ;;returns all security entries
       vars: (i dls recCount)
       (setq recCount (length _SecurityDB))
       (setq dls "")
       (loop for i from 0 until recCount do
           (setq currentUser _SecurityDB[i 1])
           (setq dls (append dls currentUser.userName (char 09)))
           ) ;;end i loop
       dls) ;; eop getSecurityInfo
    (defun getUserSecurity(userName)
       ;; returns the security record of a user as a TDS
       vars: (dls)
       (setq currentUser _SecurityDB[userName])
       (setq dls "")
       (if (<> currentUser.userName #void)
          (setq dls (append dls currentUser.userName (char 9) 
                                currentUser.userPassword (char 9)
                                currentUser.userLevel (char 9)
                                currentUser.userScript (char 9)
                                currentUser.userOnline (char 9)))
       )
       dls) ;; eop getUserSecurity
    (defun getSecurityInfo()
       ;;returns all security entries
       vars: (i dls recCount)
       (setq recCount (length _SecurityDB))
       (setq dls "")
       (loop for i from 0 until recCount do
           (setq currentUser _SecurityDB[i 1])
           (setq dls (append dls currentUser.userName (char 09)))
           (setq dls (append dls currentUser.userPassword (char 09)))
           (setq dls (append dls currentUser.userLevel (char 09)))
           (setq dls (append dls currentUser.userScript (char 09)))
           (setq dls (append dls currentUser.userOnline (char 09)))
           ) ;;end i loop
       dls) ;; eop getSecurityInfo
    (defun loginUser(userName loginSW) 
       ;; Return the User Security Record
       (setq currentUser _SecurityDB[userName])
       (setq currentUser.userOnline loginSW)
       (setq _SecurityDB[userName] currentUser)
       loginSW)
    (defun updateUser(currentName newName password securityLevel bootscript) 
       ;; Update the User Security Record
       (setq _SecurityDB[currentName] #void)
       (createUserStructure)
       (setq currentUser.userName newName)
       (setq currentUser.userPassword password)
       (setq currentUser.userLevel securityLevel)
       (setq currentUser.userScript bootscript)
       (setq _SecurityDB[newName] currentUser)            
       true) ;; eop updateUser     
    ;; Initialize the security Lambda
    Continue::
    (setq myParent (myself))
    (createUserStructure)
    (if (= (length _SecurityDB) 0) (clearDataBase))
    true) ;; end of securityLambda


































;;**EXPORTKEY**:segmentLambda
(defun segmentLambda(...)
;; *******************************************************************
;;  Summary: This Lambda is in charge of History database screening and
;;           development of History Database Screening Strategies which
;;           maximize the specified measurement criteria and are
;;           constrained by the specified exclusion criteria.
;; Args:     none
;; Return:   true 
;; *******************************************************************
   pvars:(;; Persistent variables
          autoGenSW                 ;; True if we wish to auto generate criteria.
          (geneticProgramming true) ;; True if Genetic Programming is to be used during Thinking.
          measureCriteriaName       ;; The measure criteria name.
          (multivariateRegression true) ;; True if Multivariate Regression is to be used during Thinking.
          passIndex                 ;; The index of this criteria generation pass (see think).
          (passMax 01)              ;; The maximum number of criteria generation passes (see think).
          rowsInDisplay             ;; The number of rows in the score display.
          (ruleInduction true)      ;; True if Rule Induction is to be used during Thinking.
          ;; Methods list
          delimitedScores           ;; Return a tab delimited string of scores.
          delimitedScoreTitles      ;; Return a tab delimited string of score titles.
          errorStop                 ;; Recover from any normal errors.
          rowCount                  ;; Return the number of rows in score display.
          runCriteria               ;; Run a single History ad hoc query.
          runSelection              ;; Run a single History screening strategy.
          startThinking             ;; Start History screening strategy development.
          sysErrorStop              ;; Recover from any system errors.
          think                     ;; Develop History screening strategies.
          ) ;; end persistent variables
   vars:(selection strategy)
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> startThinking #void) (goto Continue:))
   (defun delimitedScores(measureName selectCriteriaName)
   ;; Note: See also screenMeasureScoreVector, which performs a
   ;;       similar function during the measurement process.
   ;; Display the scores in the History database for the 
   ;; specified measurement criteria and selection criteria.
       vars:(scores dls keyName theSpec 
             detailTable tableRows colName
             actualCount rowIndex
             ndata navg nsum nmin nmax nstd nrar nnum ncnt 
             ) ;; end Temporary variables
       ;; First create a nil display to show if the specified
       ;; selection criteria have not been measured yet.
       (setq scores (append "Avg" #\tab "NA" _eol))
       (setq scores (append scores "Sum" #\tab "NA" _eol))
       (setq scores (append scores "Min" #\tab "NA" _eol))
       (setq scores (append scores "Max" #\tab "NA" _eol))
       (setq scores (append scores "Std" #\tab "NA" _eol))
       (setq scores (append scores "Rar" #\tab "NA" _eol))
       (setq scores (append scores "Num" #\tab "NA" _eol))
       (setq rowsInDisplay 7)
       ;; Now try and load the specified selection criteria
       ;; detailed results table from the blackboard.
       (setq theSpec (selectCriteriaLambda.Pv.getCriteria selectCriteriaName))
       (setq keyName (append "screenLambda:" theSpec))
       (setq detailTable _Blackboard[keyName])
       (if (= detailTable #void) (return scores))
       ;; Compute the scores for the specified selection
       ;; criteria and the specified measurement criteria.
       (setq colName (symbol measureName))
       (setq actualRows 0)
       (setq navg 0)
       (setq nsum 0)
       (setq nmin 999999999999999)
       (setq nmax -999999999999999)
       (setq nstd 0)
       (setq nrar 0)
       (setq nnum 0)
       (setq dls "")
       (setq tableRows detailTable.rowCount)
       (if (= tableRows 0) (return scores))
       (loop for rowIndex from 0 until tableRows do
           (setq ndata detailTable[colName rowIndex])
           (setq dls (append dls detailTable[Date: rowIndex] #\tab (text ndata "#.##0") _eol))
           (setq ncnt detailTable[Size: rowIndex])
           (+= nnum ncnt)
           (if (and (<> ndata #void) (> ncnt 0))
               (begin
                  (++ actualCount)
                  (+= navg ndata)
                  (+= nstd (* ndata ndata))
                  (setq nmin (min nmin ndata))
                  (setq nmax (max nmax ndata))
                  )) ;; end if
           ) ;; end loop
       ;; Compute the scores stastics for the selection criteria
       (if (> actualCount 0)
           (begin
              (/= navg actualCount)
              (setq nsum (* navg nnum))
              (/= nstd actualCount)
              (-= nstd (* navg navg)) 
              (setq nstd (sqrt nstd))
              (if (<> nstd 0) (setq nrar (/ navg nstd))) 
              (setq scores (append "Avg" #\tab (text navg "#.##0") _eol))
              (setq scores (append scores "Sum" #\tab (text nsum "#.##0") _eol))
              (setq scores (append scores "Min" #\tab (text nmin "#.##0") _eol))
              (setq scores (append scores "Max" #\tab (text nmax "#.##0") _eol))
              (setq scores (append scores "Std" #\tab (text nstd "#.##0") _eol))
              (setq scores (append scores "Rar" #\tab (text nrar "#.##0") _eol))
              (setq scores (append scores "Num" #\tab (text nnum "####0") _eol))              
              )) ;; end if
       ;; Set the number of rows in the scores display
       (setq rowsInDisplay (addi tableRows 7))
       (setq scores (append scores dls))
       scores) ;; end delimitedScores
   (defun delimitedScoreTitles()
   ;; Note: See also screenMeasureScoreVector, which performs a
   ;;       similar function during the measurement process.
   ;; Display the score titles in the History database for the 
   ;; specified measurement criteria and selection criteria.
       vars:(scores)
       (setq scores (append "Avg" #\tab))
       (setq scores (append scores "Sum" #\tab))
       (setq scores (append scores "Min" #\tab))
       (setq scores (append scores "Max" #\tab))
       (setq scores (append scores "Std" #\tab))
       (setq scores (append scores "Rar" #\tab))
       (setq scores (append scores "Num" #\tab))
       scores) ;; end delimitedScoreTitles
   (defun errorStop(err) (writeln "segmentLambda:" err) false)
   (defun rowCount() rowsInDisplay)
   (defun runCriteria(selectQueryText viewLambda dateText)
   ;; Run a single History Database ad hoc query against 
   ;; the specified viewer Lambda for the specified date.
   ;; by the specified exclusion criteria.
       (onError (lambda(err) false))  ;; Trap all errors here.
       (viewLambda.Pv.runCriteria selectQueryText)
       true) ;; end runCriteria
   (defun runSelection(selectCriteriaName viewLambda dateText)
   ;; Run a single History Database Screening Strategy against 
   ;; the specified viewer Lambda for the specified date.
   ;; by the specified exclusion criteria.
       (onError (lambda(err) false))  ;; Trap all errors here.
       (viewLambda.Pv.runSelection selectCriteriaName)
       true) ;; end runSelection
   (defun startThinking(measureName genSW)
   ;; Start History Database Screening Strategy development.
       (onError errorStop)  ;; Trap all errors here.
       (setq _sysError sysErrorStop) ;; Trap all system errors here.
       (setq measureCriteriaName measureName)
       (setq _Blackboard[_thinkingMeasureName:] measureName)
       ;; Run one pass with no auto criteria.
       (if (= genSW false)
           (begin
              (setq autoGenSW false)
              (setq passMax 1)
           )) ;; end if
       ;; Run one pass with auto criteria being generated.
       (if (= genSW true)
           (begin
              (setq autoGenSW true)
              (setq passMax 1)
           )) ;; end if
       ;; Run multiple passes with auto criteria being generated.
       (if (isNumber genSW)
           (begin
              (setq autoGenSW true)
              (setq passMax genSW)
           )) ;; end if
       (think 0)
       (setq _sysError #void)
       true) ;; end startThinking
   (defun sysErrorStop(errCode)
   ;; Recover from any system errors which occur.
       (writeln "segmentLambda got a system error #" errCode)
       (if (isMember errCode #(1005 1012 1013)) (return true))
       (gc)
       (think errCode)) ;; end sysErrorStop
   (defun think(errCode)
   ;; Develop History Database Screening Strategies which maximize 
   ;; the specified measurement criteria and are constrained
   ;; by the specified exclusion criteria.
       vars:(newCount)
       (onError errorStop)  ;; Trap all errors here.
       (setq _sysError sysErrorStop) ;; Trap all system errors here.
       ;; Initialize if this is the first time think has been called.
       (if (= errCode 0)
           (begin
              ;(errWriteln.reset) ;; MFK Testing
              (writeln "Begin preparation for thinking.")
              (setq passIndex 0)
              (historyViewer.doClear)
              (measureCriteriaLambda.loadUnfairFields measureCriteriaName)
              )) ;; end if
       (if (and (<> errCode 0) (= autoGen true)) 
           (begin
              (writeln "[" passIndex " of " passMax "] Restarting after error.")
              (goto RestartAfterError:)))
       ;; Remeasure all the current selection criteria.
       ;; Weed out all the unfit selection criteria.
       ;; Seed new selection criteria (if not too full).
       ;; Rescore all existing Blackboard criteria against all measurement criteria.
       (writeln "[-] Rescore all selection criteria against available measurement criteria.")
       (screenBacktest)
       ;; Return if machine generated criteria are not requested.
       (if (= autoGenSW false) (goto CompleteFinalPass:))
       StartNewPass::           
          ;(errWriteln.reset) ;; MFK Testing
          ;; Weed out all machine generated criteria whose scores are too low.
          (writeln "[-] Weed out all machine generated criteria whose " measureCriteriaName " score is too low.")
          (setq newCount (selectCriteriaLambda.weedCriteria measureCriteriaName))
          ;; Machine generated criteria via Genetic programming factory Lambdas
          (if (= geneticProgramming true)
              (begin
                 (writeln "[" passIndex " of " passMax "] Genetic programming to maximize " measureCriteriaName " score.")
                 (selectCriteriaLambda.seedCriteria measureCriteriaName newCount)
                 )) ;; end if
          ;; Machine generated criteria via Multivariate regression factory Lambdas
          (if (= multivariateRegression true)
              (begin
                 (writeln "[" passIndex " of " passMax "] Multivariate regression to maximize " measureCriteriaName " score.")
                 (selectCriteriaLambda.addAutoCriteria (regressLambda 1 5 measureCriteriaName))
                 (selectCriteriaLambda.addAutoCriteria (regressLambda 1 5 measureCriteriaName))
                 (selectCriteriaLambda.addAutoCriteria (regressLambda 1 5 measureCriteriaName))
                 (selectCriteriaLambda.addAutoCriteria (regressLambda 1 5 measureCriteriaName))
                 (selectCriteriaLambda.addAutoCriteria (regressLambda 1 5 measureCriteriaName))
                 )) ;; end if
          ;; Machine generated criteria via Rule Induction factory Lambdas
          (if (= ruleInduction true)
              (begin
                 (writeln "[" passIndex " of " passMax "] Rule induction to maximize " measureCriteriaName " score.")
                 )) ;; end if
          ;; Machine generated criteria via User Specified factory Lambdas
          (if (<> _runUserFactoryLambdas #void)
              (begin
                 (_runUserFactoryLambdas passIndex passMax measureCriteriaName)
                 )) ;; end if
          ;; Rescore all existing Blackboard criteria against all measurement criteria.
          RestartAfterError::
          (writeln "[" passIndex " of " passMax "] Rescore all selection criteria against available measurement criteria.")
          (screenBacktest)
          (++ passIndex)
          (if (< passIndex passMax) (goto StartNewPass:))
       ;; Complete the final pass and return to caller.
       CompleteFinalPass::
       (setq _sysError #void)
       true) ;; end startThinking
   ;; Initialization
   Continue::
   (measureCriteriaLambda)
   (selectCriteriaLambda)
   true) ;; end segmentLambda





















;;**EXPORTKEY**:selectCriteriaLambda
(defun selectCriteriaLambda()
;; *******************************************************************
;;  Summary: This Lambda is in charge of History database selection
;;           Screening Strategies which maximize the available
;;           measurement criteria and are constrained by the
;;           available exclusion criteria.
;; Args:     none
;; Return:   myTView     The tableview of the selection criteria. 
;; *******************************************************************
    pvars:(selectST                  ;; The selection criteria Smarttable
           myParent                  ;; The parent Lambda object reference
           myTView                   ;; The selection criteria Tableview
           nameSerial                ;; A serial number for generating automated names
           sortLambda                ;; The sort lambda value for use in sorting criteria
           (maxCriteria 1000)        ;; Maximum selection criteria allowed in blackboard
           (maxAutoCriteria 100)     ;; Maximum automated selection criteria generated in blackboard
           (primeGeneration 20)      ;; Maximum top scoring auto selection criteria kept in blackboard from prior generation.
           (sexualGeneration 80)     ;; Maximum sexual generation auto selection criteria grown in blackboard for new generation.
           (mutantGeneration 10)     ;; Maximum mutant generation auto selection criteria grown in blackboard for new generation.
           (newGeneration 10)        ;; Maximum new generation auto selection criteria grown in blackboard for new generation.
           (sortByStatisticsIndex 0) ;; Measurement statistics index to sort on (see screenMeasureScoreVector)
           (sortByStatisticsSwitch 0);; Measurement statistics sort descending(0) ascending(1) switch.
           ;; Child Lambdas
           addAutoCriteria           ;; Add the specified new automated criteria to the blackboard
           addNewCriteria            ;; Add the specified new criteria to the blackboard
           Checkin
           clearCriteriaScores       ;; Clear all criteria measurement scores from the Blackboard
           deleteCriteria            ;; Delete the specified criteria from the blackboard
           doClear
           dropCriteria              ;; Remove all criteria from the Blackboard
           dropAutoCriteria          ;; Remove all machine generated criteria from the Blackboard
           findCriteriaName          ;; Return the selection criteria with the specified unique name. 
           findCriteriaSpecification ;; Return the selection criteria with the specified unique Specification. 
           getColumnHeaders          ;; Return the History database valid column headers
           getCriteria               ;; Return the selection criteria strategy specification string
           loadCondition  
           loadCriteriaName          ;; Load list of all criteria names for display in the names list box. 
           loadCriteriaNotes         ;; Return the selection criteria notes string
           loadFieldName  
           loadLogic 
           loadSort  
           loadValue
           measureSortIndexGet       ;; Get the measure sort by index value (see screenMeasureScoreVector).
           measureSortSwitchGet      ;; Get the measure sort by switch value, descending(0) ascending(1).
           measureSortIndexSet       ;; Set the measure sort by index value (see screenMeasureScoreVector).
           measureSortSwitchSet      ;; Set the measure sort by switch value, descending(0) ascending(1).
           ref2
           refColCount
           refColKeyness
           refColName
           refColType
           refRowCount
           renameCriteria            ;; Rename the specified criteria
           saveCriteriaNotes         ;; Save the specified criteria notes
           set2
           seedCriteria              ;; Give birth to new selection criteria using genetic programming techniques
           sortCriteria              ;; Sort the selection criteria descending on the specified measurement
           weedCriteria              ;; Remove unfit selection criteria using the spcified measurement
           weedNullCriteria          ;; Remove all null selection criteria using the spcified measurement
           ) ;; end of persistant variables
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> ref2 #void) (goto Continue:))
    ;; Initialize the inline child Lambdas.
    (defun addAutoCriteria(newSpec)
    ;; Add the specified new criteria to the blackboard.
       vars:(rowIndex nameIndex newName)
       ;; If no user name, generate a new name automatically.
       (++ nameSerial)
       (setq newName (append "_Auto__" nameSerial "_" (now)))
       ;; Load selection strategies table (if necessary).
       (if (= selectST #void) (selectCriteriaLambda))      
       ;; See if the selection criteria table is already full.
       (if (>= selectST.rowCount maxCriteria) (return false))
       ;; See if the automated criteria specification is already in use.
       (setq rowIndex (findCriteriaSpecification newSpec))
       (if (isNumber rowIndex) (return false))
       ;; See if the user criteria name is already in use.
       (setq nameIndex (findCriteriaName newName))
       (if (isNumber nameIndex) (return false))
       ;; Add the selection criteria to the blackboard table.
       (setq rowIndex selectST.rowCount)
       (addRow selectST 1 rowIndex)
       (setq selectST[Specification: rowIndex] newSpec)
       (setq selectST[Notes: rowIndex] #void)
       (setq selectST[Name: rowIndex] newName)
       true) ;; end addAutoCriteria
    (defun addNewCriteria(newSpec newName newNotes)
    ;; Add the specified new criteria to the blackboard.
       vars:(rowIndex nameIndex)
       ;; If no user name, generate a new name automatically.
       (if (= newName #void)
           (setq newName (append "_Strategy_" (now)))
           ) ;; end if
       ;; Load selection strategies table and set up long transaction.
       (beginTransaction _Blackboard) 
       (setq selectST (screenLoadStrategies))      
       ;; See if the selection criteria table is already full.
       (if (>= selectST.rowCount maxCriteria)
           (begin
              (abortTransaction _Blackboard)
              (error "databaseFull")
           )) ;;end if
       ;; See if the user criteria specification is already in use.
       (setq rowIndex (findCriteriaSpecification newSpec))
       (if (isNumber rowIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "CriteriaInUse")
           )) ;;end if
       ;; See if the user criteria name is already in use.
       (setq nameIndex (findCriteriaName newName))
       (if (isNumber nameIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "NameInUse")
           )) ;;end if
       ;; Add the selection criteria and its detail results table (if any).
       (deleteView myTView)
       (setq myTView #void)
       (setq rowIndex selectST.rowCount)
       (addRow selectST 1 rowIndex)
       (setq selectST[Specification: rowIndex] newSpec)
       (setq selectST[Notes: rowIndex] newNotes)
       (setq selectST[Name: rowIndex] newName)
       (setq _Blackboard.screenLambda selectST)
       (commitTransaction _Blackboard)
       (setq myTView (^new Tableview: selectST))
       newName) ;; end addNewCriteria
    (defun Checkin(title) (writeln "Checked in " title))
    (defun deleteCriteria(criteriaName) 
    ;; Delete the specified criteria from the blackboard.
       vars:(rowIndex detailTableKey)
       (beginTransaction _Blackboard) 
       (setq selectST (screenLoadStrategies))      
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "UnknownName")
           )) ;;end if
       ;; Delete the selection criteria and its detail results table (if any).
       (setq detailTableKey (append "screenLambda:" selectST[rowIndex].Specification))
       (setq _Blackboard[detailTableKey] #void)
       (deleteView myTView)
       (setq myTView #void)
       (deleteRow selectST 1 rowIndex)
       (setq _Blackboard.screenLambda selectST)
       (commitTransaction _Blackboard)
       (setq myTView (^new Tableview: selectST))
       true) ;; end saveCriteriaNotes
    (defun doClear() (setq selectST (new Smarttable:)))
    (defun findCriteriaName(name)
    ;; Return the selection criteria with the specified unique name. 
       vars:(rowIndex rowCount nameString)
       (if (= selectST #void) (selectCriteriaLambda)) ;; Make sure we're initialized.
       (setq nameString (string name))
       (loop for rowIndex from 0 until selectST.rowCount do
          (if (= nameString selectST[Name: rowIndex])
              (return rowIndex))
          ) ;; end loop
       false) ;; end findCriteriaName
    (defun findCriteriaSpecification(specString)
    ;; Return the selection criteria with the specified unique Specification. 
       vars:(rowIndex rowCount nameString)
       (if (= selectST #void) (selectCriteriaLambda)) ;; Make sure we're initialized.
       (loop for rowIndex from 0 until selectST.rowCount do
          (if (= specString selectST[Specification: rowIndex])
              (return rowIndex))
          ) ;; end loop
       false) ;; end findCriteriaSpecification
    (defun getColumnHeaders(startCol endCol) 
    ;; Return the History database valid column headers.
        vars:(i structLen dls cellValue)  
        ;;no value need as it is an RTTI
        (setq dls "")
        (setq structLen (ref (length _validFields)))
        (loop for i from 0 until structLen do
            (setq dls (append dls (ref _validFields [i 0]) (char 09)))
            ) ;;end i loop
        (append dls ""))
    (defun getCriteria(criteriaName)
    ;; Return the selection criteria strategy specification string
    ;; saved under the specified unique name.
       vars:(rowIndex result)
       (if (= selectST #void) (selectCriteriaLambda)) ;; Make sure we're initialized.
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (setq result "")
           (setq result selectST[Specification: rowIndex])
           ) ;;end if
       result) ;; end getCriteria
    (defun loadCondition() 
       vars:(conditionString)
       (setq conditionString "")
       (setq conditionString (append "and" (char 09) "or" (char 09) "<" (char 09) "<=" (char 09) "=" (char 09)))
       (setq conditionString (append conditionString ">" (char 09) ">=" (char 09) "<>" (char 09))))
    (defun loadCriteriaName(...)
    ;; Load list of all criteria names for display in the names list box. 
       vars:(rowIndex rowCount nameString measureName fieldName)
       ;; Make sure we're initialized.
       (if (= selectST #void) (selectCriteriaLambda))
       (findAll myTView)
       (setq nameString "")
       ;; Load optional sort argument. If present,
       ;; use it to presort the selection criteria names.
       (if (= (argCount) 1)
           (begin
              (setq fieldName (symbol (argFetch 0)))
              (setq sortLambda.Pv.fieldName fieldName)
              (setq sortLambda.Pv.sortIndex sortByStatisticsIndex)
              (sort myTView sortLambda)
              (loop for rowIndex from 0 until myTView.rowCount do
                 ;(if (and (<> myTView[|Last Date|: rowIndex] #void) (<> myTView[fieldName rowIndex] #void))
                     (setq nameString (append nameString myTView[Name: rowIndex] (char 09)))
                 ;    ) ;; end if
                 ) ;; end loop
              ) ;; end then
           (begin
              (sort myTView (lambda(x y) (<=  x.Name  y.Name)))
              (loop for rowIndex from 0 until myTView.rowCount do
                 (setq nameString (append nameString myTView[Name: rowIndex] (char 09)))
                 ) ;; end loop
              ) ;; end else
           ) ;; end if
       nameString) ;; end loadCriteriaName
    (defun loadCriteriaNotes(criteriaName) 
    ;; Return the selection criteria notes string
    ;; saved under the specified unique name.
       vars:(rowIndex result) 
       (if (= selectST #void) (selectCriteriaLambda)) ;; Make sure we're initialized.
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (setq result "")
           (setq result selectST[Notes: rowIndex])
           ) ;;end if
       result) ;; end loadCriteriaNotes
    (defun loadFieldName() 
       vars:(columnHeaders)
       (setq columnHeaders (myParent.getColumnHeaders 0 (myParent.refColCount))))
    (defun loadLogic() (append "and" (char 09) "or" (char 09)))
    (defun loadSort() 
       vars:(sortString)
       (setq sortString "")
       (setq sortString (append  sortString "all" (char 09) "top" (char 09)))
       (setq sortString (append  sortString "bottom" (char 09) "omit" (char 09) "rule")))
    (defun loadValue() 
       vars:(valueString)
       (setq valueString ""))
    (defun measureSortIndexGet() sortByStatisticsIndex)
    (defun measureSortSwitchGet() sortByStatisticsSwitch)
    (defun measureSortIndexSet(newIndex) (setq sortByStatisticsIndex newIndex))
    (defun measureSortSwitchSet(newIndex)
       (setq sortByStatisticsSwitch newIndex)
       (if (= newIndex 0)
           ;; Set sort for descending
           (setq sortLambda (eval '(lambda (x y) 
                               vars:(xvalue yvalue)
                               pvars:(fieldName sortIndex)
                               (if (or (= x.|Last Date| #void) (= x.Size #void) (= x.Size 0))
                                   (setq xvalue #void) 
                                   (setq xvalue x[fieldName][sortIndex]))
                               (if (or (= y.|Last Date| #void) (= y.Size #void) (= y.Size 0))
                                   (setq yvalue #void) 
                                   (setq yvalue y[fieldName][sortIndex]))
                               (>= xvalue yvalue))))
           ;; Set sort for ascending
           (setq sortLambda (eval '(lambda (x y) 
                               vars:(xvalue yvalue)
                               pvars:(fieldName sortIndex)
                               (if (or (= x.|Last Date| #void) (= x.Size #void) (= x.Size 0))
                                   (setq xvalue #void) 
                                   (setq xvalue x[fieldName][sortIndex]))
                               (if (or (= y.|Last Date| #void) (= y.Size #void) (= y.Size 0))
                                   (setq yvalue #void) 
                                   (setq yvalue y[fieldName][sortIndex]))
                               (<= xvalue yvalue))))
           ) ; end if
       ) ; end measureSortSwitchSet
    (defun ref2(col row) myTView[col row])
    (defun refColCount() myTView.colCount)
    (defun refColKeyness(col) myTView.colVector[col].colFormat)
    (defun refColName(col) myTView.colVector[col].name)
    (defun refColType(col) "G")
    (defun refRowCount() myTView.rowCount)
    (defun renameCriteria(oldName newName) 
    ;; Rename the specified criteria. Make sure the
    ;; new criteria name is unique.
       vars:(oldIndex newIndex)
       (beginTransaction _Blackboard) 
       (setq selectST (screenLoadStrategies))      
       (setq newIndex (findCriteriaName newName))
       (if (isNumber newIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "NameInUse")
           )) ;;end if
       (setq oldIndex (findCriteriaName oldName))
       (if (isBoolean oldIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "UnknownName")
           )) ;;end if
       (setq selectST[Name: oldIndex] (string newName))
       (setq _Blackboard.screenLambda selectST)
       (commitTransaction _Blackboard)
       newName) ;; end renameCriteria
    (defun saveCriteriaNotes(criteriaName newNotes) 
    ;; Save the specified criteria notes.
       vars:(rowIndex)
       (beginTransaction _Blackboard) 
       (setq selectST (screenLoadStrategies))      
       (setq rowIndex (findCriteriaName criteriaName))
       (if (isBoolean rowIndex)
           (begin
              (abortTransaction _Blackboard)
              (error "UnknownName")
           )) ;;end if
       (setq selectST[Notes: rowIndex] (string newNotes))
       (setq _Blackboard.screenLambda selectST)
       (commitTransaction _Blackboard)
       true) ;; end saveCriteriaNotes
    (defun set2(col row newValue) (set myTView col row newValue))
    ;; Initialize the selection Lambda
    Continue::
    (setq selectST (screenLoadStrategies))
    (setq myParent (myself))
    (setq myTView (new Tableview: selectST))
    (measureSortSwitchSet 0)
    ) ;; end of selectCriteriaLambda




















;;**EXPORTKEY**:selectCriteriaLambda:clearCriteriaScores
(defChildLambda selectCriteriaLambda:clearCriteriaScores()
;; *******************************************************************
;;  Summary: Clear all selection criteria measurement scores from the 
;;           Blackboard.
;; Args:     none
;; Return:   rowCount   Number of selection criteria.    
;; *******************************************************************
    vars:(rowIndex rowCount detailTableKey)
    (measureClearAll)
    ;; Initialize the selection Lambda
    (if (= selectST #void) (selectCriteriaLambda)) ;; Make sure we're initialized.
    (setq rowCount selectST.rowCount)
    (loop for rowIndex from 0 until rowCount do
        ;; Drop all automated criteria which we have seen.
        (setq detailTableKey (append "screenLambda:" selectST[rowIndex].Specification))
        (setq _Blackboard[detailTableKey] #void)
        (setq selectST[rowIndex].|Last Date| #void)
        (setq selectST[rowIndex].Size #void)
        ) ;; end loop
    (setq _Blackboard.screenLambda selectST)
    (commitTransaction _Blackboard)
    ;; Return the number of strategies remaining.
    (setq rowCount selectST.rowCount)
    rowCount) ;; end of selectCriteriaLambda:clearCriteriaScores




















;;**EXPORTKEY**:selectCriteriaLambda:dropAutoCriteria
(defChildLambda selectCriteriaLambda:dropAutoCriteria()
;; *******************************************************************
;;  Summary: Remove all machine generated selection Screening Strategies 
;;           from the Blackboard, leaving only the human selection Strategies.
;; Args:     none
;; Return:   autoSeenCount The number of machine generated criteria dropped.            
;; *******************************************************************
    vars:(rowIndex rowCount nameList nameIndex nameCount)
    ;; Initialize the selection Lambda
    (if (= selectST #void) (selectCriteriaLambda)) ;; Make sure we're initialized.
    (setq rowCount myTView.rowCount)
    ;; Record all machine generated selection criteria names. These
    ;; automated selection criteria will be dropped from the system.
    (setq nameList (new Vector: rowCount))
    (setq nameIndex 0)
    (loop for rowIndex from 0 until rowCount do
        ;; Drop all automated criteria which we have seen.
        (if (= (left myTView[rowIndex].Name 7) "_Auto__")
            (begin
               (setq nameList[nameIndex] myTView[rowIndex].Name)
               (++ nameIndex))
            ) ;; end if
        ) ;; end loop
    (if (<= nameIndex 0) (return nameIndex))
    (resize nameList nameIndex)
    ;; Eliminate all automated selection criteria.
    (setq nameCount nameIndex)
    (beginTransaction _Blackboard) 
    (loop for nameIndex from 0 until nameCount do
        (setq rowIndex (findCriteriaName nameList[nameIndex]))
        (if (isNumber rowIndex)
            (begin
               (setq detailTableKey (append "screenLambda:" selectST[rowIndex].Specification))
               (setq _Blackboard[detailTableKey] #void)
               (deleteRow selectST 1 rowIndex)
            )) ;; end if
        ) ;; end loop
    (setq _Blackboard.screenLambda selectST)
    (commitTransaction _Blackboard)
    ;; Return the number of new strategies to create.
    (setq rowCount selectST.rowCount)
    nameCount) ;; end of selectCriteriaLambda:dropAutoCriteria




















;;**EXPORTKEY**:selectCriteriaLambda:dropCriteria
(defChildLambda selectCriteriaLambda:dropCriteria()
;; *******************************************************************
;;  Summary: Remove all selection Screening Strategies from the Blackboard.
;; Args:     none
;; Return:   droppedCount The number of criteria dropped.            
;; *******************************************************************
    vars:(rowIndex rowCount nameList nameIndex nameCount)
    ;; Initialize the selection Lambda
    (if (= selectST #void) (selectCriteriaLambda)) ;; Make sure we're initialized.
    (setq rowCount myTView.rowCount)
    ;; Record all machine generated selection criteria names. These
    ;; automated selection criteria will be dropped from the system.
    (setq nameList (new Vector: rowCount))
    (setq nameIndex 0)
    (loop for rowIndex from 0 until rowCount do
        ;; Drop all criteria which we have seen.
        (setq nameList[nameIndex] myTView[rowIndex].Name)
        (++ nameIndex)
        ) ;; end loop
    (if (<= nameIndex 0) (return nameIndex))
    (resize nameList nameIndex)
    ;; Eliminate all automated selection criteria.
    (setq nameCount nameIndex)
    (beginTransaction _Blackboard) 
    (loop for nameIndex from 0 until nameCount do
        (setq rowIndex (findCriteriaName nameList[nameIndex]))
        (if (isNumber rowIndex)
            (begin
               (setq detailTableKey (append "screenLambda:" selectST[rowIndex].Specification))
               (setq _Blackboard[detailTableKey] #void)
               (deleteRow selectST 1 rowIndex)
            )) ;; end if
        ) ;; end loop
    (setq _Blackboard.screenLambda selectST)
    (commitTransaction _Blackboard)
    ;; Return the number of new strategies to create.
    (setq rowCount selectST.rowCount)
    nameCount) ;; end of selectCriteriaLambda:dropCriteria




















;;**EXPORTKEY**:selectCriteriaLambda:seedCriteria
(defChildLambda selectCriteriaLambda:seedCriteria(measureCriteriaName count)
;; *******************************************************************
;;  Summary: Give birth to History database selection Screening Strategies 
;;           which try to maximize the specified measurement criteria.
;; Args:     measureCriteriaName  The name of the measure criteria.
;;           count                The number of new criteria to create.
;; Return:   true
;; *******************************************************************
    vars:(newList newIndex newCount 
          oldList oldIndex oldCount
          lastCount
          ) ;; end of temporary variables
    ;; Sort the selection criteria by the specified measurement criteria.
    (if (<= count 0) (return true))
    (sortCriteria measureCriteriaName)
    (setq nameSerial 1)
    ;; Initialize the selection Lambda
    (if (= selectST #void) (selectCriteriaLambda)) ;; Make sure we're initialized.
    ;; Record the specifications for the prime generation of top selection criteria.
    (setq oldCount (find myTView 0 (subi primeGeneration)))
    (setq oldList (new Vector: primeGeneration))
    (loop for oldIndex from 0 until oldCount do
        (setq oldList[oldIndex] myTView[oldIndex].Specification)
        ) ;; end loop
    ;; If there are fewer than primeGeneration, then create some new ones.
    (if (<= oldCount primeGeneration)
        (loop for oldIndex from oldCount until primeGeneration do
        (setq oldList[oldIndex] (screenBirth 1 7))
            ) ;; end loop
        ) ;; end if
    (setq oldCount primeGeneration)
    ;; Create new strategies using sexual reproduction.
    (setq newList (new Vector: count))
    (setq newCount (min count sexualGeneration))
    (loop for newIndex from 0 until newCount do
        (setq newList[newIndex] (screenReproduce oldList[(integer (random oldCount))] oldList[(integer (random oldCount))]))
        ) ;; end loop
    (setq lastCount newCount)
    (setq count (subi count newCount))
    ;; Create new strategies using asexual mutation.
    (setq newIndex (min count mutantGeneration))
    (setq count (subi count newIndex))
    (setq newCount (addi newCount newIndex))
    (loop for newIndex from lastCount until newCount do
        (setq newList[newIndex] (screenMutate oldList[(integer (random oldCount))]))
        ) ;; end loop
    (setq lastCount newCount)
    ;; Create new strategies using spontaneous mutation.
    (setq newIndex (min count newGeneration))
    (setq count (subi count newIndex))
    (setq newCount (addi newCount newIndex))
    (loop for newIndex from lastCount until newCount do
        (setq newList[newIndex] (screenBirth 1 7))
        ) ;; end loop
    ;; Save all automated selection criteria to the blackboard.
    (loop for newIndex from 0 until newCount do
        (addAutoCriteria newList[newIndex])
        ) ;; end loop
    (setq _Blackboard.screenLambda selectST)
    ;; Return the number of new strategies to create.
    true) ;; end of selectCriteriaLambda:seedCriteria




















;;**EXPORTKEY**:selectCriteriaLambda:sortCriteria
(defChildLambda selectCriteriaLambda:sortCriteria(measureCriteriaName)
;; *******************************************************************
;;  Summary: Sort the History database selection Screening Strategies 
;;           to maximize the specified measurement criteria and
;;           return the sorted tableview.
;; Args:     measureCriteriaName  The name of the measure criteria to sort on.
;; Return:   myTView              The sorted tableview of the selection criteria. 
;; *******************************************************************
    vars:(rowIndex colName sortLambda)
    ;; Locate the specified measurement critera by its name.
    (setq rowIndex (measureCriteriaLambda.findCriteriaName measureCriteriaName))
    (if (isBoolean rowIndex) (error "measureName" ))
    ;; Initialize the selection Lambda
    (if (= selectST #void) (selectCriteriaLambda)) ;; Make sure we're initialized.
    ;; Sort the selection criteria by the specified measurement criteria.
    (setq colName (symbol measureCriteriaName))
    (setq sortLambda.Pv.fieldName (symbol colName))
    (setq sortLambda.Pv.sortIndex sortByStatisticsIndex)
    (findAll myTView)
    (sort myTView sortLambda)
    myTView) ;; end of selectCriteriaLambda:sortCriteria




















;;**EXPORTKEY**:selectCriteriaLambda:weedCriteria
(defChildLambda selectCriteriaLambda:weedCriteria(measureCriteriaName)
;; *******************************************************************
;;  Summary: Remove unfit History database selection Screening Strategies 
;;           which fail to maximize the specified measurement criteria.
;; Args:     measureCriteriaName  The name of the measure criteria.
;; Return:   count                The number of new criteria to create.             
;; *******************************************************************
    vars:(rowIndex rowCount nameList nameIndex nameCount autoSeenCount)
    ;; Initialize the selection Lambda
    (if (= selectST #void) (selectCriteriaLambda)) ;; Make sure we're initialized.
    ;; Sort the selection criteria by the specified measurement criteria.
    (setq measureCriteriaName (symbol measureCriteriaName))
    (sortCriteria measureCriteriaName)
    (setq rowCount myTView.rowCount)
    ;; Record all automated selection criteria names after the first ones,
    ;; plus any automated selection criteria with measurements of zero.
    ;; These automated selection criteria will be weeded from the system.
    (setq nameList (new Vector: rowCount))
    (setq nameIndex 0)
    (setq autoSeenCount 0)
    (loop for rowIndex from 0 until rowCount do
        ;; Count all automated criteria which we have seen.
        (if (= (left myTView[rowIndex].Name 7) "_Auto__") (++ autoSeenCount))
        ;; Keep the prime generation of top selection criteria, unless there
        ;; are automated criteria with zero for the specified measurement.
        (if (and (= (left myTView[rowIndex].Name 7) "_Auto__")
                 (>= autoSeenCount primeGeneration)
                 ) ;; end and
            (begin
               (setq nameList[nameIndex] myTView[rowIndex].Name)
               (++ nameIndex))
            ) ;; end if
        ) ;; end loop
    (if (<= nameIndex 0) (return (min (subi maxCriteria rowCount) maxAutoCriteria)))
    (resize nameList nameIndex)
    ;; Eliminate all automated selection criteria after the prime generation.
    (setq nameCount nameIndex)
    (beginTransaction _Blackboard) 
    (loop for nameIndex from 0 until nameCount do
        (setq rowIndex (findCriteriaName nameList[nameIndex]))
        (if (isNumber rowIndex)
            (begin
               (setq detailTableKey (append "screenLambda:" selectST[rowIndex].Specification))
               (setq _Blackboard[detailTableKey] #void)
               (deleteRow selectST 1 rowIndex)
            )) ;; end if
        ) ;; end loop
    (setq _Blackboard.screenLambda selectST)
    (commitTransaction _Blackboard)
    ;; Return the number of new strategies to create.
    (setq rowCount selectST.rowCount)
    (min (subi maxCriteria rowCount) maxAutoCriteria)) ;; end of selectCriteriaLambda:weedCriteria




















;;**EXPORTKEY**:selectCriteriaLambda:weedNullCriteria
(defChildLambda selectCriteriaLambda:weedNullCriteria(measureCriteriaName)
;; *******************************************************************
;;  Summary: Remove null History database selection Screening Strategies 
;;           which fail to maximize the specified measurement criteria.
;; Args:     measureCriteriaName  The name of the measure criteria.
;; Return:   count                The number of new criteria to create.             
;; *******************************************************************
    vars:(rowIndex rowCount nameList nameIndex nameCount autoSeenCount)
    ;; Initialize the selection Lambda
    (if (= selectST #void) (selectCriteriaLambda)) ;; Make sure we're initialized.
    ;; Sort the selection criteria by the specified measurement criteria.
    (setq measureCriteriaName (symbol measureCriteriaName))
    (sortCriteria measureCriteriaName)
    (setq rowCount myTView.rowCount)
    ;; Record all automated selection criteria names after the first ones,
    ;; plus any automated selection criteria with measurements of zero.
    ;; These automated selection criteria will be weeded from the system.
    (setq nameList (new Vector: rowCount))
    (setq nameIndex 0)
    (setq autoSeenCount 0)
    (loop for rowIndex from 0 until rowCount do
        ;; Count all automated criteria which we have seen.
        (if (= (left myTView[rowIndex].Name 7) "_Auto__") (++ autoSeenCount))
        ;; Keep the prime generation of top selection criteria, unless there
        ;; are automated criteria with zero for the specified measurement.
        (if (and (= (left myTView[rowIndex].Name 7) "_Auto__")
                 (or (= myTView[rowIndex][measureCriteriaName] 0)
                     (= myTView[rowIndex][measureCriteriaName] #void)
                     (= myTView[rowIndex].Size 0)
                     (= myTView[rowIndex].Size #void)
                     (>= autoSeenCount primeGeneration)
                     ) ;; end or
                 ) ;; end and
            (begin
               (setq nameList[nameIndex] myTView[rowIndex].Name)
               (++ nameIndex))
            ) ;; end if
        ) ;; end loop
    (if (<= nameIndex 0) (return (min (subi maxCriteria rowCount) maxAutoCriteria)))
    (resize nameList nameIndex)
    ;; Eliminate all automated selection criteria after the prime generation.
    (setq nameCount nameIndex)
    (beginTransaction _Blackboard) 
    (loop for nameIndex from 0 until nameCount do
        (setq rowIndex (findCriteriaName nameList[nameIndex]))
        (if (isNumber rowIndex)
            (begin
               (setq detailTableKey (append "screenLambda:" selectST[rowIndex].Specification))
               (setq _Blackboard[detailTableKey] #void)
               (deleteRow selectST 1 rowIndex)
            )) ;; end if
        ) ;; end loop
    (setq _Blackboard.screenLambda selectST)
    (commitTransaction _Blackboard)
    ;; Return the number of new strategies to create.
    (setq rowCount selectST.rowCount)
    (min (subi maxCriteria rowCount) maxAutoCriteria)) ;; end of selectCriteriaLambda:weedNullCriteria




















;;**EXPORTKEY**:sentenceToList
(defun sentenceToList(aCmd)
;; *******************************************************************
;; summary:  Parses a user entered natural language sentence into a
;;           structured list of word phrases. An internal Dictionary 
;;           of parser keywords is given. Each parser keyword is given
;;           a precedence. The keywords and their precedence determine
;;           the parsing action.
;; Args:     aCmd                       The natural language sentence.
;; Return:   A structured list of word phrases. 
;; *******************************************************************
    pvars:(keys brk vec lenVec ix)
    ;; Function:  Get the precedence of the current input word.
    (defun getPrecedence()
       vars:(prec)
       ;; Save the precedence of the lefthand word. 
       (setq prec keys[(downcase (copy vec[ix]))])
       (if (= prec #void) 
           (setq prec 100)
           (downcase vec[ix]))
       prec) ;; end getPrecedence 
    ;; Function: Recursively create a list with items of the same
    ;;           precedence appended at the same level.
    ;; Note:     Remember the higer the precedence score the lower
    ;;           is the importance of the word. 
    (defun groupByPrecedence(topPrec)
       vars:(leftPrec rightPrec ret)
       ;; Start the list with the lefthand word. 
       (setq leftPrec (getPrecedence))
       (setq ret (list vec[ix]))
       ;; Scan the next righthand word examining its precedence.
       (++ ix)
       (while (< ix lenVec) do
           ;; Save the precedence of the righthand word.
           (setq rightPrec (getPrecedence))
           (cond 
               ;; Append items of equal precedence to the list, at the same level.
               ((= leftPrec rightPrec)
                (begin (setq ret (append ret (list vec[ix]))) (++ ix)))     
               ;; Return the list if the righthand is lower than the top precedence.
               ((>= topPrec rightPrec)
                (return ret))
               ;; Append groups of higher precedence to the list as a lower level branch.
               ((< leftPrec rightPrec)
                (setq ret (append ret (list (groupByPrecedence leftPrec)))))     
               ;; Promote groups of inbetween precedence to the top of the list.
               ((> leftPrec rightPrec)
                (begin (setq leftPrec rightPrec) (setq ret (list vec[ix] ret)) (++ ix)))     
               ) ;; end cond     
           ) ;; end while
       ret) ;; end groupByPrecedence
       ;; Build the Dictionary of keywords (if necessary).
    (if (= keys #void)
        (begin
           (setq keys (new Dictionary:))
           (setq keys["help"] 0) 
           (setq keys["show"] 0) 
           (setq keys["want"] 0) 
           (setq keys["what"] 0) 
           (setq keys["who"] 0) 
           (setq keys["does"] 0) 
           (setq keys["equal"] 0) 
           (setq keys["to"] 0) 
           (setq keys["are"] 0) 
           (setq keys["is"] 0) 
           (setq keys["has"] 0) 
           (setq keys["you"] 0) 
           (setq keys["your"] 2) 
           (setq keys["me"] 0) 
           (setq keys["i"] 0) 
           (setq keys["of"] 1) 
           (setq keys["from"] 1) 
           (setq keys["the"] 2) 
           (setq keys["a"] 2) 
           (setq keys["an"] 2) 
           (setq keys["every"] 2) 
           (setq keys["all"] 2) 
                                )) ;; end if
       ;; Build the word break character string (if necessary).
    (if (= brk #void)
        (setq brk (append " ,.!?;:" (char 9) (char 8) (char 10) (char 13))))
    ;; Convert the sentence into a vector of words.
    (setq vec (stringToVector aCmd brk true))
    (setq lenVec (length vec)) 
    ;; Group the input words into a precedence structured list.
    (if (= lenVec 0)
        (return #void))
    (setq ix 0)
    (groupByPrecedence 0)) ;; end of sentenceToList

































;;**EXPORTKEY**:show
(defun show(thing)
;; *******************************************************************
;; summary:  This procedure shows the interior of a procedure, or
;;           the system statistics.
;; args:     thing        (Procedure) => The procedure to be shown.   
;;                        (Workspace) => The workspace Lambdas.   
;;                        (nn)        => Shows *error*[nn].   
;;                        (true)      => The workspace statistics.   
;; *******************************************************************
    vars:(s (title "") (instruction ""))
    ;; Do we show an error tree element?
    (if (isInteger thing)
        (setq thing *error*[thing]))
    ;; Do we show an error tree SPair?
    (if (isSpair thing)
        (begin
          (setq instruction (append "[" thing[1] "]"))
          (setq thing thing[0])))
    ;; Do we show an error tree Procedure Symbol?
    (if (or (isSymbol thing) (isString thing))
        (setq thing (eval thing)))
    ;; Do we disassemble a Procedure?
    (if (isType Lambda: thing)
        (begin
           (setq title (globalBinding thing))
           (if (= title #void)
               (setq title (string thing)))
     (setq title (append "" title instruction))
           (setq s (append "Variable declarations:" #\newline))
           (setq s (append s " Av = " (string thing.Av true) #\newline))
           (setq s (append s " Tv = " (string thing.Tv true) #\newline))
           (setq s (append s " Pv = " (string thing.Pv true) #\newline))
           (setq s (append s "Instructions:" #\newline))
           ;(setq s (append s (disassemble thing srcOnly: short:)))
           (setq s (append s (disassemble thing short:)))
           (setq s (stringToVector s (append "" #\return #\newline) true))))
    ;; Do we display Workspace Lambdas?
    (if (isType Workspace: thing)
        (begin
           (setq title "Active Workspace Views")
           (setq s (inspect _currentViews))))
    ;; Do we display Workspace statistics?
    (if (= true thing)
        (begin
           (setq title "Current Workspace Statistics")
           (setq s (inspect true))))
    ;; Do we have an improper argument?
    (if (= s #void)
        (error "showArgument"))
    (browse title "Hit OK when done browsing." s 0))






























;;**EXPORTKEY**:statisticsLambda
(defun statisticsLambda(legacyTable)
;; *******************************************************************
;; summary:  Statistics Lambda which collects and presents statistics
;;           on the current legacy database.
;; Args:     none   
;; Return:   true
;; *******************************************************************
  pvars:(myParent                ;; Parent Lambda object reference
         myIndex                 ;; Field statistics Dictionary (indexed by field name) 
         myFieldName             ;; Current field name
         myFieldStats            ;; Statistics records for the current field
         myLegacyTable           ;; Statistics legacy table
         ;; Constants
         (minValue -900000000000000000000)
         (maxValue  900000000000000000000)
         ;; Methods
         clearDataBase           ;; Clears the statistics database
         clearFieldStatistics    ;; Clears the statistics for the current field
         createFieldStructure    ;; Creates the structure used to store field statistics
         deleteField             ;; Deletes the field statistics record
         doClear                 ;; Clears the field statistics
         doFieldStatistics       ;; Compute the field statistics for the specified field
         doFieldWrapup           ;; Compute the final field statistics for the specified field
         doStatistics            ;; Compute the field statistics for the specified record
         getFieldNames           ;; Return a tab delimited string of all registered field names
         getFieldStatistics      ;; Return a tab delimited string of field statistics
         getFieldDelimitedString ;; Return a tab delimited string of field name and values
         getFieldValue           ;; Return the value of the registered field
         loadIndex               ;; Load the field statistics index
         parseFieldName          ;; Parse and save the field name
         saveIndex               ;; Saves the field statistics index
         setCurrent              ;; Sets myFieldStats
         updateField             ;; Update the field statistics record
         )   ;;end of persistant variables
    vars:(fieldIndex fieldCount)  
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> clearDataBase #void) (goto Continue:))
    ;; Initialize the inline child Lambdas.
    (defun clearDataBase()
       (setq myIndex (^new Dictionary:))
       (setq _Blackboard._Statistics myIndex)
       0) ;; end clearDataBase
    (defun clearFieldStatistics() 
       ;; Clear the current field statistics
       vars:(valueIndex valueList valueCount)  
       ;; We assume myFieldStats is loaded at this point.
       (if (= myFieldStats #void) (error "unknownField"))
       (setq myFieldStats.count #void)
       (setq myFieldStats.mean #void)
       (setq myFieldStats.min #void)
       (setq myFieldStats.max #void)
       (setq myFieldStats.std #void)
       (if (<> (setq valueList myFieldStats.valueCounts) #void)
           (begin
              (setq valueCount (length valueList))
              (loop for valueIndex from 0 until valueCount do
                  (setq dls valueList[valueIndex 0] #void)
                  ) ;;end value loop
              )) ;; end if
       true)
    (defun createFieldStructure()
       (setq myFieldStats (makeStructure name:        #void 
                                         firstName:   #void 
                                         middleName:  #void 
                                         lastName:    #void 
                                         type:        #void 
                                         count:       #void 
                                         mean:        #void 
                                         min:         #void 
                                         max:         #void 
                                         std:         #void 
                                         valueCounts: #void))
       ) ;; end of createFieldStructure
    (defun deleteField(fieldName) 
       (loadIndex)  
       (setq myIndex[fieldName] #void)
       (saveIndex)) ;; end of deleteField
    (defun doClear() 
       ;; Clear all the field statistics
       vars:(fieldIndex fieldCount)  
       (loadIndex)
       (setq fieldCount (length myIndex))
       (loop for fieldIndex from 0 until fieldCount do
           (setq myFieldStats myIndex[fieldIndex 1])
           (clearFieldStatistics)
           (setq myIndex[myIndex[fieldIndex 0]] myFieldStats)
           ) ;;end value loop
       (saveIndex)
       true) ;; end doClear
    (defun doFieldStatistics(theRecord) 
       ;; Compute the current field statistics
       vars:(valueIndex valueList valueCount key fieldValue)  
       ;; We assume myFieldStats is loaded at this point.
       (if (= myFieldStats #void) (error "unknownField"))
       (setq fieldValue (getFieldValue theRecord))
       (if (= fieldValue #void) (return true))
       (if (= myFieldStats.type Number:)
           (begin
              (if (not (isNumber fieldValue)) (return true))
              (+= myFieldStats.mean (getFieldValue theRecord))
              (if (= myFieldStats.min #void)
                  (setq myFieldStats.min (getFieldValue theRecord))
                  (setq myFieldStats.min (min myFieldStats.min (getFieldValue theRecord))))
              (if (= myFieldStats.max #void)
                  (setq myFieldStats.max (getFieldValue theRecord))
                  (setq myFieldStats.max (max myFieldStats.max (getFieldValue theRecord))))
              (+= myFieldStats.std (* (getFieldValue theRecord) (getFieldValue theRecord)))
              (++ myFieldStats.count)
              (if (<> (setq valueList myFieldStats.valueCounts) #void)
                  (begin
                     (setq valueCount (length valueList))
                     (loop for valueIndex from 0 until valueCount do
                         (if (<= (getFieldValue theRecord) (setq key valueList[valueIndex 0]))
                             (begin
                                (++ valueList[key])
                                (return true)
                                )) ;; end if
                         ) ;;end value loop
                     )) ;; end inner if
              )) ;; end number if
       (if (= myFieldStats.type Coded:)
           (begin
              (if (= fieldValue #void) (return true))
              (if (<> (setq valueList myFieldStats.valueCounts) #void)
                  (begin
                     (++ valueList[(getFieldValue theRecord)])
                     )) ;; end if
              (++ myFieldStats.count)
              )) ;; end coded if
       true)
    (defun doFieldWrapUp() 
       ;; Compute the final field statistics
       vars:(valueIndex valueList valueCount key)  
       ;; We assume myFieldStats is loaded at this point.
       (if (= myFieldStats #void) (error "unknownField"))
       (if (and (= myFieldStats.type Number:) (<> myFieldStats.count #void))
           (begin
              (/= myFieldStats.mean myFieldStats.count)
              (/= myFieldStats.std myFieldStats.count)
              (setq myFieldStats.std (- myFieldStats.std (* myFieldStats.mean myFieldStats.mean)))
              (setq myFieldStats.std (sqrt myFieldStats.std))
              )) ;; end number if
       true)
    (defun doStatistics(theRecord) 
       ;; Compute the field statistics for the specified record
       vars:(fieldIndex fieldCount)  
       (setq fieldCount (length myIndex))
       (loop for fieldIndex from 0 until fieldCount do
           (setq myFieldStats myIndex[fieldIndex 1])
           (doFieldStatistics theRecord)
           (setq myIndex[myIndex[fieldIndex 0]] myFieldStats)
           ) ;;end value loop
       true) ;; end doStatistics
    (defun getFieldDelimitedString(fieldName) 
       ;; Return a tab delimited string of field name and discrete values
       vars:(dls valueIndex valueList valueCount)  
       (loadIndex)
       (setq dls "")
       (setq myFieldStats myIndex[fieldName])
       (if (= myFieldStats #void) (error "unknownField"))
       (setq dls (append dls myFieldStats.name #\tab))
       (setq dls (append dls myFieldStats.type))
       (if (<> (setq valueList myFieldStats.valueCounts) #void)
           (begin
              (setq valueCount (length valueList))
              (loop for valueIndex from 0 until valueCount do
                  (setq dls (append dls #\tab valueList[valueIndex 0]))
                  ) ;;end value loop
              )) ;; end if
       dls) ;; end getFieldDelimitedString
    (defun getFieldNames() 
       ;; Return the User Names
       vars:(fieldIndex indexCount dls)  
       (loadIndex)
       (setq dls "")
       (setq indexCount (length myIndex))
       (loop for fieldIndex from 0 until indexCount do
           (setq myFieldStats myIndex[fieldIndex 1])
           (setq dls (append dls myFieldStats.name (char 09)))
           ) ;;end field loop
       (append dls "")) ;; end getFieldNames
    (defun getFieldStatistics(fieldName) 
       ;; Return the field statistics as a tab delimited string
       vars:(dls valueIndex valueList valueCount)  
       (loadIndex)
       (setq dls "")
       (setq myFieldStats myIndex[fieldName])
       (if (= myFieldStats #void) (error "unknownField"))
       (setq dls (append "count = " myFieldStats.count #\tab))
       (setq dls (append dls "type = " myFieldStats.type #\tab))
       (if (= myFieldStats.type Number:)
           (begin
              (setq dls (append dls #\tab "mean = " myFieldStats.mean))
              (setq dls (append dls #\tab "min = " myFieldStats.min))
              (setq dls (append dls #\tab "max = " myFieldStats.max))
              (setq dls (append dls #\tab "std = " myFieldStats.std))
              )) ;; end if
       (if (<> (setq valueList myFieldStats.valueCounts) #void)
           (begin
              (setq valueCount (length valueList))
              (setq dls (append dls #\tab "Value Counts"))
              (loop for valueIndex from 0 until valueCount do
                  (setq dls (append dls #\tab valueList[valueIndex 0] " = " valueList[valueIndex 1]))
                  ) ;;end value loop
              )) ;; end if
       dls)
    (defun getFieldValue(theRecord) 
       ;; Return the registered field value
       (if (= myFieldStats.lastName #void) (return theRecord[myFieldStats.name]))
       theRecord[myFieldStats.firstName][myFieldStats.middleName][myFieldStats.lastName] 
       ) ;; end getFieldValue
    (defun loadIndex()
       (setq myIndex _Blackboard._Statistics)
       (if (= myIndex #void) (clearDataBase))
       true) ;; end loadIndex
    (defun parseFieldName(fieldName)
       vars:(vec ftype)
       (loadIndex)
       (setq myFieldStats myIndex[fieldName])
       (if (= myFieldStats #void) (createFieldStructure))
       (setq myFieldStats.name fieldName)
       (setq vec (stringToVector fieldName "["))
       (setq myFieldStats.firstName (symbol vec[0]))
       (if (> (length vec) 1)
           (begin
              (setq myFieldStats.middleName (symbol (stringToVector vec[1] "].")[0]))
              (setq myFieldStats.lastName (symbol (stringToVector vec[1] "].")[1]))
              )
           (begin
              (setq myFieldStats.middleName #void)
              (setq myFieldStats.lastName #void)
              )) ;; end if
       (if (isMember myFieldStats.firstName _validFields)
           (setq ftype _validFields[myFieldStats.firstName])
           (error "unknownField"))
       (if (and (= ftype |isDictionary|:) (<> _parseFieldName #void))
           (setq ftype (_parseFieldName myFieldStats)))
       (cond
           ((= ftype |isNumber|:) (setq myFieldStats.type Number:))
           ((= ftype |isSymbol|:) (setq myFieldStats.type Coded:))
           (else (setq myFieldStats.type Other:))
           ) ;; end cond
       true) ;; end parseFieldName
    (defun saveIndex() 
       (if (= myIndex #void) (clearDataBase))
       (setq _Blackboard._Statistics myIndex)
       true) ;; end saveIndex
    (defun setCurrent(fieldName) 
       ;; Load myFieldStats
       (loadIndex)
       (setq myFieldStats myIndex[fieldName])
       (if (= myFieldStats #void) (error "noStatOnField"))
       true) ;; end setCurrent
    (defun updateField(fieldName valueList)
       (parseFieldName fieldName)
       (if (isVector valueList) (setq valueList (sort valueList <)))
       (setq myFieldStats.count #void)
       (setq myFieldStats.mean #void)
       (setq myFieldStats.min #void)
       (setq myFieldStats.max #void)
       (setq myFieldStats.std #void)
       (cond
           ((= myFieldStats.type Number:) (setq myFieldStats.valueCounts valueList))
           ((= myFieldStats.type Coded:) (setq myFieldStats.valueCounts (^new Dictionary:)))
           (else (setq myFieldStats.valueCounts #void))
           ) ;; end cond
       (if (= (type myFieldStats.valueCounts) Vector:)
           (begin
              (setq myFieldStats.valueCounts[(length myFieldStats.valueCounts)] maxValue)
              (setq myFieldStats.valueCounts (objectToDirectory myFieldStats.valueCounts #(#void)))
           )) ;; end if
       (setq myIndex[fieldName] myFieldStats)
       (saveIndex)
       true) ;; end updateField
    ;; We compute the field statistics for the legacy database.
    ;; We call the legacy librarian to perform the statistics run.
    Continue::
    (setq myParent (myself))
    (setq myLegacyTable legacyTable)
    (myLegacyTable.runStatistics doStatistics)
    ;; Compute final wrapup statistics
    (setq fieldCount (length myIndex))
    (loop for fieldIndex from 0 until fieldCount do
        (setq myFieldStats myIndex[fieldIndex 1])
        (doFieldWrapUp)
        (setq myIndex[myIndex[fieldIndex 0]] myFieldStats)
        ) ;;end field loop
    (saveIndex)
    true) ;; end of statisticsLambda







































;;**EXPORTKEY**:stockGetMatch
(defun stockGetMatch(name ST)
;; *******************************************************************
;;  summary: Returns the Quarterly History Smarttable index for the
;;           specified stock symbol.
;;  args:    The symbol of the stock.
;;           ST      The Quarterly History tableLambda.
;; *******************************************************************
   vars:((low 0) (high 0) (mean 0) ST STlen i index)
   ;;  Prepare the specified Stock symbol.
   (setq name (upcase (trim name)))
   ;; Perform binary search on the Smarttable symbol column
   (begin
      (setq STlen ST.rowCount)
      (setq high (-1+ STlen))
      (setq mean (divi (1+ (- high low)) 2))
      (while (<= low high) do
             (if (= ST[Symbol: mean] name)
             (begin
                 (setq index mean)
                 (setq low (1+ high)))
                 (if (< ST[Symbol: mean] name)
                     (begin
                         (setq low (1+ mean))
                         (setq mean (+ low (divi (1+ (- high low)) 2))))
                     (begin
                         (setq high (-1+ mean))
                         (setq mean (+ low (divi (1+ (- high low)) 2))))))))
   ;;  Return the specified matching Smarttable Index (if any).
   index) ;; end stockGetMatch


























;;**EXPORTKEY**:tableLambda
(defun tableLambda()
;; ********************************************************************
;; summary:  This Lambda creates, maintains, and operates as an in 
;;           memory relational table. The relational table is a series
;;           of attributed rows. The table can be sorted, we can have 
;;           multiple views, subsets of records can be found, etc.
;; Parms:    none  
;; return:   true
;; ********************************************************************
   pvars:(activeIndex              ;; Currently-active index -- used to get rows by key
          colCount 	           ;; The number of columns
          colVector                ;; Structure of attribute names (columns)
          exportSW                 ;; True if an exportTab is in progress
          fileID                   ;; Contains a fileID if an importTab is in progress
          indexSW                  ;; True if indexes are to be automatically updated
          importSW                 ;; True if an importTab is in progress
          myIndexes                ;; Dictionary of indexes
          myParent                 ;; The parent table Lambda
          rowCount      	     ;; The number of rows
          rowVector                ;; Vector of attributed relation rows
          saveVector               ;; Vector of saved relational rows
          showLimit                ;; The maximum number of records to show
          sparseSW                 ;; True if tableLambda manages sparse records
          uniqueIndexSW            ;; True if tableLambda enforces unique indexes
          ;; Methods list =====================================================
          addColumn 	           ;; Adds a new column name to the relation
          addRow 	                 ;; Adds a new row to the relation
          clearRows	           ;; Clears all the rows in the relation
          defColumns 	           ;; Defines a new column structure for the relation
          delimitedCells           ;; Return a tab delimited string of cell values
          doAddRow	           ;; Inserts an empty row to the relation
          doCellText	           ;; Enters text data into the cell row and column specified
          doClear	                 ;; Clears all the rows and columns in the relation
          doDeleteRow	           ;; Deletes a row from the relation
          doFind	                 ;; Find a matching row value in the relational table
          doIndex                  ;; Create and an index on the specified column
          doReindex                ;; Recreate all existing indexes
          doSort                   ;; Sort the relation on the specified column
          errorStop                ;; Clean up after any errors which may occur during operations
          exportTab                ;; Export a tab delimited .sbf format file into the relation
          find                     ;; Select matching rows in the relational table
          findAll                  ;; Restore the original rows in the relational table
          findRow                  ;; Return the first row with the specified value
          forEach                  ;; Operate on and possibly alter each row in a table Lambda
          getColumnHeaders         ;; Return a tab delimited string of column headers
          getRowBlock              ;; Return a tab delimited string of row values for a specified column
          getRowByKey              ;; Return a vector of rows for a given key value
          importTab                ;; Import a tab delimited .sbf format file into the relation
          len                      ;; Return the number of rows in the relation
          new                      ;; Create a new relational table
          ref1                     ;; Return a single row from the relational table
          ref2                     ;; Return a single cell from the relational table
          refExport                ;; Return a single export formatted row from the relational table
          refFormattedRow          ;; Return a single display formatted row from the relational table
          refColCount              ;; Return the number of columns in the relational table
          refRowCount              ;; Return the number of rows in the relational table
          saveState                ;; Save the current row Vector state as the permanent state
          saveSwap                 ;; Swap the row Vector during save-load
          set1                     ;; Import a record during the importTab operation
          set2                     ;; Set a single cell of the relational table
          setIndexTo               ;; Set the activeIndex 
          setIndexUpdate           ;; Set the automatic update of indexes on/off
          setImport                ;; Called by ImportTab to installed a filled vector into the relational Table
          setSparse                ;; Set the tableLambda for sparse records
          setUniqueIDX             ;; Set the tableLambda for unique indexes
          show                     ;; Display the rows in the relation
          smarttableToTableLambda   ;; Save the specified Smarttable into the table Lambda.
          sort                     ;; Sort the rows in the relation
          truncate                 ;; Truncate rows in the relational table
          truncateEnds             ;; Truncate rows in the relational table
          updateIndex              ;; Update index for specified column with new value
          updateRowByKey           ;; Make sure a row for the specified key exists
          ) ;; end of persistent variables
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> addColumn #void) (goto Continue:))
   ;; Initialize the inline child Lambdas.
   (defun addColumn(...)
      vars:(colName colIndex)
      (if (= colVector #void) (setq colVector (^new Structure:)))
      (loop for colIndex from 0 until (argCount) do
          (setq colName (symbol (argFetch colIndex)))
          (if (not (isMember colName colVector)) 
              (setq colVector (append colVector (makeStructure colName #void))))
          ) ;; end loop
      (setq colCount (length colVector))
      colCount) ;; end addColumn
   (defun clearRows()
      (setq rowVector (^new Vector: object: 0))
      (setq saveVector #void)
      (setq rowCount 0)
      (if (> (length myIndexes) 0)
         (setq myIndexes (objectToDictionary myIndexes (^new Vector: 0))))
      true) ;; end clearRows
   (defun defColumns(newColumns)
      (doClear)
      (if (isVector newColumns)
          (setq colVector (objectToStructure newColumns #(#void)))
          (setq colVector (objectToStructure newColumns))
          ) ;; end new column coercion if
      (setq colCount (length colVector))
      colCount) ;; end defColumns
   (defun delimitedCells(startRow endRow startCol endCol ...)
      ;; WJH - added optional arg to trigger 1-based row numbers
      ;; WJH - added optional arg to suppress row numbers
      vars:(rowIndex colIndex dls tab cellValue record) 
      ;; Return a tab delimited string of cell values for
      ;; all cells between the following rows and columns.
      (setq dls "")
      (setq tab (append "" (char 09)))
      (if (> startRow endRow) (return dls))
      (if (<= rowCount startRow) (return dls))
      (loop for rowIndex from startRow to endRow do
         ;; WJH - optioal row number suppression
         (if (= (argCount) 6)
            () ;; do nothing
         else
            ;; WJH - optional 1-based row numbers
            (setq dls (append dls (if (= (argCount) 5) (+ rowIndex 1) rowIndex) (char 09)))
         )
         (setq record (refFormattedRow rowIndex))
         (loop for colIndex from startCol until endCol do
             (setq cellValue record[colIndex])
             (if (isString cellValue) (setq cellValue (substitute cellValue tab " ")))
             (if (= cellValue #void) (setq cellValue ""))
             (if (isVector cellValue) (setq cellValue (string cellValue true)))
             ;(setq cellValue (append "" cellValue)) 
             ;(setq cellValue (substitute cellValue (char 09)  " "))
             ;(setq cellValue (substitute cellValue (char 13)  " "))
             (setq dls (append dls cellValue (char 09)))
         ) ;;end column loop
         (setq dls (append dls (char 10) (char 13)))
      ) ;;end row loop
      (append dls "")) ;; end of delimitedCells
   (defun doAddRow(rowIndex colIndex) (error "NoAccess"))
   (defun doCellText(rowIndex colIndex newValue) (error "NoAccess"))
   (defun doClear()
      (setq colVector #void)
      (setq rowVector (^new Vector: object: 0))
      (setq saveVector #void)
      (setq colCount 0)
      (setq rowCount 0)
      (setq myIndexes (^new Dictionary:))
      (setq activeIndex #void)
      true) ;; end doClear
   (defun doDeleteRow(rowIndex colIndex) (error "NoAccess")) ;; WJH - be sure to update indexes
   (defun doFind(colName startRow value)
      vars:(rowIndex colIndex)
      (setq colIndex (symbol colName))
      ;; Return the first row with the chosen value in the column.
      (loop for rowIndex from 0 until rowCount do
          (if (= value (string rowVector[rowIndex][colIndex]))
              (return rowIndex))
          ) ;; end of loop
      ;; If we get to here, we did not find the specified value anywhere.
      startRow) ;; end doFind
   (defun doReindex() 
      vars:(i) 
      (loop for i from 0 until (length myIndexes) do 
         (doIndex myIndexes[i 0]))) ;; end doReindex
   (defun doSort(columnName sortOrder) 
      vars: (source Lambda)
      (setq source (append "(lambda(x y) (" sortOrder "  x.|") )
      (setq source (append source columnName "| y.|"))
      (setq source (append source columnName "|))"))
      (setq Lambda (eval source))
      (sort Lambda)
      ;; Update indexes if switch is set true
      (if (= true indexSW) (doReindex))
      rowCount) ;; end doSort
   (defun errorStop(errMsg)
      (setq importSW false)
      (setq exportSW false)
      (if (isNumber fileID) 
          (fileClose fileID 1))
      (setq fileID #void)
      (error (mid errMsg 1 (subi (length errMsg) 2)))) ;; end errorStop
   (defun exportTab(fileName)
      vars:(rowIndex newIndex n vec)
      ;; Make sure we catch all errors which might occur.
      ;(onError errorStop)
      ;; Make sure we are properly initialized.
      (if (= myParent #void) (error "tableInit"))
      (if (<> myParent.Pv (myself)[Pv:]) (error "tableInit"))
      (setq exportSW true)
      ;; Open the specified .SBF file and export.
      (setq fileID (fileOpen fileName 1 0))
      (^exportTab fileID myParent recordVectors:)
      (fileClose fileID 1)
      (setq fileID #void)
      (setq exportSW false)
      rowCount) ;; end exportTab
   (defun find(findLambda)
      vars:(rowIndex newIndex n vec)
      (if (= saveVector #void) (setq saveVector (copy rowVector)))
      (if (<= rowCount 0) (return rowCount))
      ;; Select only those rows for which the find Lambda returns true.
      (setq n (length rowVector))
      (setq vec (^new Vector: object: 0))
      (setq newIndex -1)
      (loop for rowIndex from 0 until n do
          (if (findLambda rowVector[rowIndex]) (setq vec[(++ newIndex)] rowVector[rowIndex]))
          ) ;; end loop
      (setq rowVector vec)
      (setq rowCount (length rowVector))
      rowCount) ;; end find
   (defun findAll()
      (if (= saveVector #void) (setq saveVector (copy rowVector)))
      (setq rowVector (copy saveVector))
      (setq rowCount (length rowVector))
      rowCount) ;; end findAll
   (defun findRow(col value)
      vars:(rowIndex colIndex)
      (setq colIndex (makeSymbol col))
      ;; Return the first row with the chosen value in the column.
      (loop for rowIndex from 0 until rowCount do
          (if (= value (string rowVector[rowIndex][colIndex]))
              (return rowIndex))
          ) ;; end of loop
      ;; If we get to here, we did not find the specified value anywhere.
      false) ;; end of findRow
   (defun forEach(rowLambda)
      vars:(rowIndex newIndex n vec)
      (if (<= rowCount 0) (return rowCount))
      ;; Operate on and possibly alter each row in the table.
      (setq n (length rowVector))
      (loop for rowIndex from 0 until n do
          (setq rowVector[rowIndex] (rowLambda rowVector[rowIndex]))
          ) ;; end loop
      rowCount) ;; end find
   (defun getColumnHeaders(startCol endCol) 
      vars:(colIndex dls colName)  
      (setq dls "")
      (loop for colIndex from startCol until endCol do
          (setq colName (string colVector[colIndex 0]))
          (if (= (left colName 1) "|") (setq colName (mid colName 1 (subi (length colName) 2))))
          (setq dls (append dls colName (char 09)))
          ) ;;end column loop
      (append dls "")) ;; end getColumnHeaders
   (defun getRowBlock(startRow endRow colName) 
      vars:(rowIndex dls cellValue)  
      (setq dls "")
      (setq startRow (max startRow 0))
      (setq endRow (min endRow rowCount))
      (loop for rowIndex from startRow until endRow do
          (setq dls (append dls rowVector[rowIndex][colName] (char 09)))
          ) ;;end row loop
      (append dls "")) ;; end getRowBlock
   (defun importTab(fileName)
      vars:(rowIndex newIndex n vec)
      ;; Make sure we catch all errors which might occur.
      ;; (onError errorStop)
      ;; Make sure we are properly initialized.
      (if (= myParent #void) (error "tableInit"))
      (if (<> myParent.Pv (myself)[Pv:]) (error "tableInit"))
      (setq importSW true)
      ;; Open the specified .SBF file and import.
      (setq fileID (fileOpen fileName 0 0))
      (^importTab fileID myParent recordVectors:)
      (fileClose fileID 1)
      (setq fileID #void)
      (setq importSW false)
      (findAll)
      rowCount) ;; end importTab
   (defun len() 
      (setq rowCount (length rowVector))
      rowCount) ;; end len
   (defun new()
      (setq colVector #void)
      (setq rowVector (^new Vector: object: 0))
      (setq myIndexes (^new Dictionary:))
      (setq saveVector #void)
      (setq colCount 0)
      (setq rowCount 0)
      (setq showLimit 10)
      (setq importSW false)
      (setq exportSW false)
      (setq indexSW false)
      (setq fileID #void)
      true) ;; end of new
   (defun ref1(rowOrMember)
      (if (isSymbol rowOrMember) (return (myself)[Pv:][rowOrMember])) ;; Allows Lambda polymorphism.
      (if (not (isNumber rowOrMember)) (error "invalidKey"))
      ;;;;;;;;;;;;;;;; WHJ - this section commented-out to match MFK's test Lambda
      ;; If we are importing, we will return a vector (if there are no columns defined),
      ;; or we will return an empty record Structure (if there are columns defined).
      ;(if (= importSW true)
      ;    (if (= colVector #void)
      ;        (return (^new Vector: 0))
      ;        (return (copy colVector)))
      ;    ) ;; end importing if
      ;; If we are exporting, we will return records formatted for export.
      ;(if (= exportSW true) (return (refExport rowOrMember)))
      ;;;;;;;;;;;;;
      ;; We are now retrieving a specific indexed row of the table.
      rowVector[rowOrMember]) ;; end of ref1
   (defun ref2(colName rowIndex) rowVector[rowIndex][colName])
   (defun refColCount() colCount)
   (defun refExport(rowIndex)
     vars:(record colIndex colName) 
      ;; Construct the .SBF column name header record for tab delimited export.
      (if (= rowIndex 0)
          (begin
             (setq record (^new Vector: colCount))
             (loop for colIndex from 0 until colCount do
                 (setq colName (string colVector[colIndex 0]))
                 (if (= (left colName 1) "|") (setq colName (mid colName 1 (subi (length colName) 2))))
                 (setq record[colIndex] colName)
                 ) ;; end loop
             (return record)
          )) ;; end if
      ;; Construct the .SBF column type header record for tab delimited export.
      (if (= rowIndex 1) (return (^new Vector: colCount "G")))
      ;; Construct the .SBF column keyness header record for tab delimited export.
      (if (= rowIndex 2) (return (^new Vector: colCount "n")))
      ;; We are now exporting a specific indexed row of the table.
      (setq rowIndex (subi rowIndex 3))
      (if (>= rowIndex rowCount) (return false))
      (if (= sparseSW true)
          (setq record (objectToStructure (copy colVector) rowVector[rowIndex]))
          (setq record rowVector[rowIndex])
          ) ;; end spare if
      record) ;; end of refExport 
   (defun refFormattedRow(rowIndex)
      vars:(record)
      (if (not (isNumber rowIndex)) (error "invalidKey"))
      ;; We are now retrieving a specific indexed row of the table.
      (if (= sparseSW true)
          (setq record (objectToStructure (copy colVector) rowVector[rowIndex]))
          (setq record rowVector[rowIndex])
          ) ;; end spare if
      record) ;; end of refFormattedRow
   (defun refImport(row)
      (if (not (isNumber row)) (error "invalidKey"))
      ;; Return a an empty vector (if there are no columns defined),
      ;; or return an empty record Structure (if there are columns defined).
      (if (= colVector #void)
          (return (^new Vector: 0))
          (return (copy colVector)))
      ) ;; end of refImport

   (defun refRowCount() (if (= exportSW true) (addi rowCount 3) rowCount))
   (defun saveState() (setq saveVector rowVector))
   (defun saveSwap(theVector)
      vars:(holdArea)
      (if (= saveVector #void)
          (setq holdArea rowVector)
          (setq holdArea saveVector))
      (setq rowVector theVector) 
      (setq saveVector theVector) 
      holdArea) ;; end of saveSwap
   (defun set1(rowIndex recordVector)
      vars:(newRecord)
      ;; Only valid during the importTab function.
      (if (<> importSW true) (error "NoAccess")) 
      ;; Allow numeric record keys only!
      (if (not (isNumber rowIndex)) (error "invalidKey")) 
      ;; Import the .SBF column header record.
      (if (= rowIndex 0)
          (begin 
             (setq colVector (objectToStructure recordVector #(#void)))
             (setq colCount (length colVector))
             (return true)
             )) ;; end if
      ;; Ignore all other .SBF header records.
      (if (< rowIndex 3) (return true))
      ;; Import all other .SBF records as data rows.
      (if (= sparseSW true)
          (setq newRecord (objectToDictionary recordVector))
          (setq newRecord recordVector)
          ) ;; end sparse if
      (setq rowVector[rowCount] newRecord)
      (setq rowCount (length rowVector))
      rowCount) ;; end of set1
   (defun set2(colName rowIndex newValue) 
      vars:(oldValue)
      (if (= indexSW false)
         (return (setq rowVector[rowIndex][colName] newValue))
      else
         (begin 
            (setq oldValue rowVector[rowIndex][colName])
            (setq rowVector[rowIndex][colName] newValue)
            (if (<> myIndexes[colName] #void)
               (updateIndex colName newValue oldValue rowIndex)
            )
         )
      )
      rowVector[rowIndex]) ;; end of set2
   (defun setImport(rowIndex recordVector)
      vars:(newRecord)
      ;; Only valid during the importTab function.
      (if (<> importSW true) (error "NoAccess")) 
      ;; Allow numeric record keys only!
      (if (not (isNumber rowIndex)) (error "invalidKey")) 
      ;; Import the .SBF column header record.
      (if (= rowIndex 0)
          (begin 
             (setq colVector (objectToStructure recordVector #(#void)))
             (setq colCount (length colVector))
             (return true)
             )) ;; end if
      ;; Ignore all other .SBF header records.
      (if (< rowIndex 3) (return true))
      ;; Import all other .SBF records as data rows.
      (if (= sparseSW true)
          (setq newRecord (objectToDictionary recordVector))
          (setq newRecord recordVector)
          ) ;; end sparse if
      (setq rowVector[rowCount] newRecord)
      (setq rowCount (length rowVector))
      rowCount) ;; end of setImport
   (defun setIndexTo(idxName)
      (if (<> #void myIndexes[idxName])
         (begin 
            (setq activeIndex idxName)
            (return true)
         )
      else
         (begin 
            (setq activeIndex #void)
            (return false)
         )
      )) ;; end of setIndexTo
   (defun setIndexUpdate(flag)
      (setq indexSW flag) true) ;; end of setIndexUpdate
   (defun setSparse(SW) (clearRows) (setq sparseSW SW))
   (defun setUniqueIDX(flag) 
      (setq uniqueIndexSW flag)) ;; end of setUniqueIDX
   (defun show(startIndex) 
      vars:(i n) 
      (setq n (min (length rowVector) (addi startIndex showLimit)))
      (loop for i from startIndex until n do
          (writeln "[" i "] " (refFormattedRow i))
          ) ;; end loop
      true) ;; end show
   (defun sort(sortLambda)
      (if (= saveVector #void) (setq saveVector (copy rowVector)))
      (if (<= rowCount 0) (return rowCount))
      (^sort rowVector sortLambda)
      rowCount) ;; end sort
   (defun truncate(rowLimit)
      vars:(rowIndex newIndex n vec)
      ;; Truncate the relational table to the specified number of rows.
      (setq rowLimit (max (min rowLimit (length rowVector)) 0))
      (if (<= rowCount rowLimit) (return rowCount))
      (resize rowVector rowLimit)
      (setq rowCount (length rowVector))
      ;; Reindex if flag is set
      (if (= true indexSW) (doReindex))
      rowCount) ;; end truncate
   (defun truncateEnds(begRowLimit endRowLimit)
      vars:(rowIndex newIndex n vec endRowIndex)
      (if (= saveVector #void) (setq saveVector (copy rowVector)))
      (if (<= rowCount 0) (return rowCount))
      ;; Select only those rows in between the begin and end row limits.
      (setq n (length rowVector))
      (setq endRowIndex (subi n endRowLimit))
      (setq vec (^new Vector: object: 0))
      (setq newIndex -1)
      (loop for rowIndex from begRowLimit until endRowIndex do
          (setq vec[(++ newIndex)] rowVector[rowIndex])
          ) ;; end loop
      (setq rowVector vec)
      (setq rowCount (length rowVector))
      ;; Reindex if flag is set
      (if (= true indexSW) (doReindex))
      rowCount) ;; end truncateEnds
   ;; Initialize the Lambda and clear the relational table.
   Continue::
   (setq myParent (myself))
   (new)
   myParent) ;; end of tableLambda
    








;;**EXPORTKEY**:tableLambda:addRow
(defChildLambda tableLambda:addRow(newRow)
   vars:(newRecord idxObj idxNum idxColumn idxNode)
   ;; Make a new record template and copy into it.
   (if (= sparseSW true)
       (cond 
         ((isDictionary newRow)
          (setq newRecord newRow))
         ((isStructure newRow)
          (setq newRecord (objectToDictionary newRow)))
         (else
          (setq newRecord (objectToDictionary (copy colVector) newRow)))
         ) ;; end sparse record coercion cond
       (cond 
         ((isStructure newRow)
          (setq newRecord newRow))
         (else
          (setq newRecord (objectToDictionary (copy colVector) newRow)))
         ) ;; end dense record coercion cond
       ) ;; end sparse if
   ;; Make sure we are adding to the main row vector
   (if (<> saveVector #void) 
       (begin
          (setq rowVector saveVector)
          (setq saveVector #void)
       )) ;; end if
   ;; Add new record to the end of the main row vector
   (setq rowVector[rowCount] newRecord)
   ;; Update indexes if switch is set true
   (if (= indexSW true)
      (if (> (length myIndexes) 0) 
         (loop for idxNum from 0 until (length myIndexes) do
            ;; Get key name of index
            (setq idxColumn myIndexes[idxNum 0])
            ;; Retrieve index object
            (setq idxObj myIndexes[idxNum])
            ;; Retrieve value node
            (setq idxNode idxObj[newRecord[idxColumn]])
            ;; Test to see if node found
            (if (<> #void idxNode) 
               (begin 
                  ;; Found - add new row to existing idx value node
                  (setq idxNode[(length idxNode)] rowCount)   
                  (setq idxObj[newRecord[idxColumn]] idxNode)
               )
            else
               ;; Not found - create new idx value node,
               ;; but don't index #voids 
               (if (<> #void newRecord[idxColumn])
                  (begin
                     (setq idxNode (^new Vector: 0))
                     (setq idxNode[0] rowCount)
                     (setq idxObj[newRecord[idxColumn]] idxNode)
                  )
               )
            )
            ;; Save the updated index
            (setq myIndexes[idxNum] idxObj)
         )  
      )
   )
   ;; Advance row count 
   (setq rowCount (length rowVector))
   rowCount) ;; end addRow








;;**EXPORTKEY**:tableLambda:doIndex
(defChildLambda tableLambda:doIndex(columnName)
   ;;
   vars: (row            ;; Row index into rowVector
          value          ;; Current index value
          limit          ;; rowVector loop limit
          idxVector      ;; Vector of row numbers for this index value
          thisIndex      ;; Index dictionary being built
         )
   ;;
   ;;
   ;; Initialize local variables
   (setq columnName (symbol columnName))
   (setq limit (length rowVector))
   (setq thisIndex (^new Dictionary:))
   ;;
   ;; Scan the rowVector, building the index
   (loop for row from 0 until limit do
      ;; Retrieve value
      (setq value rowVector[row][columnName])
      ;; Test to see if value is indexed
      (if (<> #void (setq idxVector thisIndex[value]))
         ;; Add this row to value node
         (setq idxVector[(length idxVector)] row)
      else
         (begin
            ;; Create new value node
            (setq idxVector (^new Vector: 0))
            ;; Index this row 
            (setq idxVector[0] row)
         )
      )
      ;; Save altered value node for this value
      (setq thisIndex[value] idxVector)
   )
   ;; Save newly-created index, overwriting any existing version 
   (setq myIndexes[columnName] thisIndex)
   true) ;; end of doIndex








;;**EXPORTKEY**:tableLambda:getRowByKey
(defChildLambda tableLambda:getRowByKey(keyValue)
;; *************************************************************************************
;; summary:  This procedure gets a vector of rows from the table Lambda. The key value must
;;           be a value for the tableLambda's current index 
;;                               
;; args:     keyValue:           The key object whose attributes are used to find the row.
;;           
;; return:   resultVector        A vector of row objects who match the key.
;;                               Length of return is zero if no rows match the key.
;;                               
;; *************************************************************************************
   vars:(
         resultVect
         idxObj
         idxNode         
         rowIndex
        )
   ;; Initialize result
   (setq resultVect (^new Vector: 0))
   ;; If there are no rows, we can't find ours.
   (if (= rowCount 0) (return resultVect))
   ;; Try to retrieve the index node for this value
   (if (<> #void (setq idxObj myIndexes[activeIndex]))
      (setq idxNode idxObj[keyValue])
   )
   ;; If value not in index, no rows can match
   (if (= #void idxNode) (return resultVect))
   ;; Build vector of matching rows
   (loop for rowIndex from 0 until (length idxNode) do
      (setq resultVect[rowIndex] rowVector[idxNode[rowIndex]])
   )
   ;; return the resultVect
   resultVect) ;; end getRowByKey
   




























;;**EXPORTKEY**:tableLambda:updateIndex
(defChildLambda tableLambda:updateIndex(colName newValue oldValue rowIndex)
;; *************************************************************************************
;; summary:  This procedure updates the specified index. Row references are removed from
;;           the previous value's node.
;;           
;; args:     colName:           The name of the column and index key
;;           newValue:          The key object for the new value's index node
;;           oldValue:          The key object for the old value's index node
;;           rowIndex:          The row number being indexed
;;           
;; return:   true        
;;                               
;; *************************************************************************************
   pvars:(idxObj idxNode rowVect oldVect)
   ;; 
   ;; Update index if it exists
   (if (= #void (setq idxObj myIndexes[colName]))
      ;; unknown index error
      (error "unknownIndex")
   else
      (begin 
         ;; Remove row from old value's node, if present
         (if (<> #void (setq oldVect idxObj[oldValue]))
            (setq idxObj[oldValue] (delete rowIndex oldVect))
         )
         ;; Retrieve index node for new value
         (setq idxNode idxObj[newValue])
         (if (<> #void idxNode) 
            ;; Do nothing if row is already indexed
            (if (= (member rowIndex idxNode) false)
               (begin 
                  ;; Add row to existing idx value node
                  (setq idxNode[(length idxNode)] rowCount)   
                  (setq idxObj[newValue] idxNode)
               )
            )
         else
            ;; Create new idx value node
            (begin
               (setq idxNode (^new Vector: 0))
               (setq idxNode[0] rowIndex)
               (setq idxObj[newValue] idxNode)
            )
         )
         ;; Save the updated index
         (setq myIndexes[colName] idxObj)
      )
   )  
   true) ;; end updateIndex








;;**EXPORTKEY**:tableLambda:updateRowByKey
(defChildLambda tableLambda:updateRowByKey(keyValue)
;; *************************************************************************************
;; summary:  This procedure gets a Row from the specified Table
;;           using the values in the key attributes of the key object. The key object
;;           may be any object which can be indexed by the active index.
;;           If there is no matching row, a new row is added with
;;           the specified key values.
;; args:     key:                The key object whose attributes are used to find the row.
;;
;; return:   row:                The Row whose key attributes match the key object,
;;                               a new Row whose key attributes are set to match 
;;                               the key object, or an error if there is more than one
;;                               Row whose key attributes match the key object.
;; *************************************************************************************
   vars:(
         row 
         resultVect
        )
   ;; If there are matching rows, then return them.
   (setq resultVect (getRowByKey keyValue))
   (if (> (length resultVect) 0) (return resultVect))
   ;;
   ;; Get the next available row index.
   (setq row (objectToDictionary colVector))
   (setq row[activeIndex] keyValue)
   (addRow row)
   (setq resultVect[0] row)
   resultVect) ;; end updateRowByKey



























;;**EXPORTKEY**:tableDbms
(defun tableDbms(objRepository)
;; *******************************************************************
;; Summary:  Database Librarian Lambda which manages a simple table 
;;           database and controls the table database schema. The 
;;           table schema is similar to the relational model except 
;;           there is only one table in the database.
;; Keys:     _colVector	        The table's vector of column names.
;;           _blockCount	The table's current block count.
;;           _blockSize 	The number of records per table block.
;;           _exportLayout   	The table's export record fields layouts.
;;           _importFixup   	The table's import record fields fixup Lambda.
;;           _name    		The table's current record count.
;;           _recordCount	The table's current record count.
;;           _recordStructure	The table's record structure.
;;           _tableDate		The table's date (assigned at creation time).
;;           _validFields   	The table's valid fields and types (necessary for query building).
;;           {Positive Number}	The record blocks are assigned sequential numbers.
;;           {Negative Number}	All temporary record blocks are assigned negative numbers.
;; Depends:  excludCriteriaLambda
;;           rulesLib
;;           prodCriteriaLambda
;;           selectCriteriaLambda
;; Args:     objRepository  The table database repository object.
;; Return:   self           The table database managers.
;; *******************************************************************
    ;; Persistent Variables
    pvars:(myOR                ;; The table object repository
           myIndex             ;; The index of the table object repository
           (myResident false)  ;; The Lambda librarian residence switch
           self                ;; The parent Lambda
           backupPathName      ;; The path and file name of the table backup database
           blockKEY            ;; The retrieval key for the current block
           (blockSize 500)     ;; The table database blocking factor
           checkPointPathName  ;; The path and file name of the table checkpoint database
           colVector           ;; The table database column vector
           currentBlock        ;; The current block of table database records
           currentExportLayout ;; The current export layout record
           exportFieldData     ;; The current export field metadata dictionary 96.06.26 WJH
           exportLayouts       ;; The current export layout dictionary
           importDate          ;; The date associated with the imported table data
           importFileID        ;; The file identifier of the table tab delimited file
           importFixup         ;; The Lambda which fixes up each imported record       
           importFixupSW       ;; True if each imported record is also to be fixed up       
           importingSW         ;; True if we are importing now switch
           importNilRows       ;; Count of nil rows which have been ignored by import
           maxRecordLimit      ;; The maximum number of table records to import
           name                ;; The name of the database table
           recordCount         ;; The count of records in the table database
           recordNUM           ;; The table database current record number
           recordStructure     ;; Blank record structure for table database records
           sbfFormatSW         ;; True if we are importing an .SBF file
           tablePathName       ;; The path and file name of the table database
           updatingDirty       ;; Update has occurred switch
           updatingSW          ;; Update transaction in progress switch
           validFields         ;; The table's valid field names and types (necessary for query building).
           ;; Private Child Lambdas
           _makeBlockList      ;; Return a list of contiguous block keys ready to be merged
           _mergeBlockList     ;; Merge a list of blocks into a sequential run of blocks
           _readBlockList      ;; Return the next record from a list of blocks
           _renameBlocks       ;; Rename the keys for the specified list of blocks
           ;; Public Child Lambdas
           abortTrans          ;; Manage repository abort transaction
           addRow              ;; Add a row to the table database
           backup              ;; Backup or restore the table database repository
           beginTrans          ;; Manage repository begin transaction
           Checkin             ;; Manage repository check in
           checkPoint          ;; CheckPoint or restore the current rows in the table database
           cnvColHeaders       ;; Convert the valid fields into a column header vector
           commitTrans         ;; Manage repository commit transaction
           copyDatabase        ;; Copy one table database to another table database (with defragmentation)
           copyFile	       ;; Copy one table database file to another
           delimitedCells      ;; Return block of tab delimited field values for display
           doAddRow            ;; Add a row to the table database
           doCellText          ;; Add text to a cell in the table database
           doClear             ;; Clear the table database
           doDeleteRow         ;; Delete a row from the table database
           doFind              ;; Find a row in the table database
           doSort              ;; Sort the table database
           exportCells         ;; Return block of tab delimited field values for export
           fieldKeys           ;; Valid keys (for specified Dictionary field) as tab delimited string
           fieldSubfields      ;; Valid subfield names (for specified Dictionary field) as tab delimited string
           fieldSubtypes       ;; Valid subfield types (for specified Dictionary field) as tab delimited string
           fieldTypes          ;; Field names and types as tab delimited string
           findAll             ;; Acts as a noop for backward compatibility (see restore)
           findRow             ;; Find a row in the table database
           fixupColumnHeaders  ;; Fix column headers if they are broken
           flush               ;; Complete any pending block write operations
           getColumnHeaders    ;; Return the column headers as a tab delimited string
           getColVector        ;; Return the table data column vector
           importFixup         ;; Fixup the imported table data record
           importLegacyData    ;; Import manager
           importTab           ;; Import manager for tab delimited data
           len                 ;; Return the number of records in the table
           queryBuilder        ;; Converts query specification text into a table query Lambda
           refColCount         ;; Return the number of columns in the table
           refColName          ;; Return the specified column name
           refColValues        ;; Return a column values vector
           refRow              ;; Return a row at random
           refRowCount         ;; Return the number of rows in the table
           refRowSeq           ;; Return a row sequentially
           ref1                ;; Return the specified row
           reset               ;; Reset the current block to #void
           restart             ;; Reset the record and block count to zero
           restore             ;; Restore all original rows in the table database
           runCriteria         ;; Run the specified selection criteria
           runProdSelection    ;; Run the specified Production selection strategy
           runSelection        ;; Run the specified selection strategy
           runStatistics       ;; Run statistics for the current table database
           select              ;; Select only records which match a lambda expression
           setRowSeq           ;; Save a row sequentially
           setup               ;; Set the database backup, checkpoint, block size, and valid fields.
           set2                ;; Set a field (not used)
           set1                ;; Add a row during import processing
           smarttableToTableDbms;; Save Smarttable into table database
           sort                ;; Sort the table database
           switchUser          ;; Restore the table database for a new user
           tableLambdaToTableDbms;; Save tableLambda into table database
           tableToHistoryLambda ;; Return table database as a history time slice tableLambda
           tableToSmarttable   ;; Return table database as a Smarttable
           tableToTableLambda   ;; Return table database as a tableLambda
           truncate            ;; Truncate rows in the table database
           updateBegin         ;; Begin an update transaction on the table database 
           updateEnd           ;; Terminate an update transaction on the table database
           updateRead          ;; Read a table database row for later update
           updateReadStructure ;; Read a table database row (converted to a Structure) for later update
           updateWrite         ;; Write a table database row previously read for update
           ) ;; end of pvars
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> ref1 #void) (goto Continue:))
    ;; Initialize the inline child Lambdas.
    (defun abortTrans() (abortTransaction myOR))
    (defun addRow(record) (setRowSeq recordCount record))
    (defun backup(backupSW)
	(if (or (= backupPathName "") (= backupPathName #void))
	    (error "tMgr:NoPath"))
        (abortTransaction myOR)
        (if backupSW
            (begin
               ;(copyFile tablePathName backupPathName)
               (copyDatabase backupPathName)
               ) ;; end user backup from table database
            (begin
               (copyFile backupPathName tablePathName)
               (beginTransaction myOR)
               (setq recordCount myIndex._recordCount)
               (setq recordStructure myIndex._recordStructure)
               (commitTransaction myOR)
               ) ;; end restore from table backup database
            ) ;; end if
        ) ;; end backup
    (defun beginTrans() (beginTransaction myOR))
    (defun Checkin(title) (error "NoAccess"))
    (defun checkPoint(checkptSW)
        (if (or (= checkPointPathName "") (= checkPointPathName #void))
            (error "tMgr:NoPath"))
        (abortTransaction myOR)
        (if checkptSW
            (begin
               ;(copyFile tablePathName checkPointPathName)
               (copyDatabase checkPointPathName)
               ) ;; end user check point from table database
            (begin
               (copyFile checkPointPathName tablePathName)
               (beginTransaction myOR)
               (setq recordCount myIndex._recordCount)
               (setq recordStructure myIndex._recordStructure)
               (commitTransaction myOR)
               ) ;; end restore from user check point
            ) ;; end if
        ) ;; end checkPoint
    (defun cnvColHeaders()
        vars:(fieldIndex fieldCount colHeaders)
	    (if (<> (type validFields) Structure:)
	        (return (^new Vector: 0)))
	    (setq fieldCount (length validFields))
        (setq colHeaders (^new Vector: fieldCount))
        (loop for fieldIndex from 0 until fieldCount do
            (setq colHeaders[fieldIndex] validFields[fieldIndex 0])
            ) ;; end loop
        colHeaders) ;; end cnvColHeaders
    (defun commitTrans() (commitTransaction myOR))
    (defun doAddRow(count row) (error "NoAccess"))
    (defun doCellText(col row value) (error "NoAccess")) 
    (defun doClear() true) ;; Only import can clear.
    (defun doDeleteRow(count row) (error "NoAccess"))
    (defun doFind(colName startRow value) (error "NoAccess"))
    (defun doSort(columnName sortOrder) (error "NoAccess"))
    (defun exportLayoutCreate()                                         ;; 96.06.26 WJH
        (setq currentExportLayout (makeStructure name:        #void 
                                                 type:        #void 
                                                 typeData:    #void
                                                 fieldList:   #void 
                                                 fieldData:   #void))
        true) ;; end of exportLayoutCreate
    (defun findAll() true)
    (defun findRow(col value) (error "NoAccess"))
    (defun fixupColumnHeaders() 
    ;; Fix column headers if they are broken.
        vars:(i j n)
        (setq colVector (cnvColHeaders))
        (setq recordStructure (objectToStructure colVector #(#void)))
        (if (<> myIndex #void) 
            (setq myIndex._colVector colVector)
            (begin
                (beginTransaction myOR) 
                (setq myIndex._colVector colVector)
                (setq myIndex._recordStructure recordStructure)
                (commitTransaction myOR)))
        colVector) ;; end fixupColumnHeaders
    (defun flush()
        ;; Do we need to write the current block?
        (if (<> currentBlock #void)
            (begin
               (setq myOR[blockKEY] currentBlock)
               (setq myIndex._recordCount recordCount)
               (setq myIndex._blockCount (add1 blockKEY))))
        true) ;; end of flush
    (defun getColumnHeaders(startCol endCol) 
        vars:(i getRow getCol dls cellValue)  
        (if (<> myIndex #void) 
            (setq colVector myIndex._colVector)
            (begin
                (beginTransaction myOR) 
                (setq colVector myIndex._colVector)
                (abortTransaction myOR)))
        ;; Return a tab delimited string for the client viewer.
        (setq dls "")
        (loop for i from startCol until endCol do
            (setq dls (append dls colVector[i] (char 09)))
            ) ;;end for loop
        (append dls "")) ;; end of getColumnHeaders
    (defun getColVector() 
        (if (<> myIndex #void) 
            (setq colVector myIndex._colVector)
            (begin
                (beginTransaction myOR) 
                (setq colVector myIndex._colVector)
                (abortTransaction myOR)))
        colVector) ;; end of getColVector
    (defun len()
        (if (<> myIndex #void) 
            (setq recordCount myIndex._recordCount)
            (begin
                (beginTransaction myOR) 
                (setq recordCount myIndex._recordCount)
                (abortTransaction myOR)))
        recordCount) ;; end of len
    (defun refColCount()
        vars:(count)
        (if (<> myIndex #void) 
            (setq count (length myIndex._colVector))
            (begin
                (beginTransaction myOR) 
                (setq count (length myIndex._colVector))
                (abortTransaction myOR)))
        count) ;; end of len
    (defun refColKeyness(col) formula:)
    (defun refColName(col)
        vars:(name)
        (if (<> myIndex #void) 
            (setq name myIndex._colVector[col])
            (begin
                (beginTransaction myOR) 
                (setq name myIndex._colVector[col])
                (abortTransaction myOR)))
        name) ;; end of refColName
    (defun refColType(col) "G")
    (defun refRow(row)
        vars:(blockKEY recordNUM block record)
        ;; Compute block and record numbers.
        (setq blockKEY (divi row blockSize))
        (setq recordNUM (modi row blockSize))
        ;; Retrieve block
        (setq block myOR[blockKEY])
        ;; Retrieve record
        (if (= block #void)
            (setq record #void)
            (setq record (objectToStructure (copy recordStructure) block[recordNUM])))
        record) ;; end of refRow
    (defun refRowCount() (len))
    (defun refRowSeq(row)
        vars:(record newBlockKEY)
        ;; Compute new block and record numbers.
        (setq newBlockKEY (divi row blockSize))
        (setq recordNUM (modi row blockSize))
        ;; Load new block of records?
        (if (or (= row 0)
                (<> newBlockKEY blockKEY))
            (begin
               (setq blockKEY (divi row blockSize))
               (setq currentBlock myOR[blockKEY])
               )) ;; end if
        ;; Retrieve record
        (if (= currentBlock #void)
            (setq record #void)
            (setq record (objectToStructure (copy recordStructure) currentBlock[recordNUM])))
        record) ;; end of refRowSeq
    (defun ref1(rowOrMember)
       ;; If we are importing, we will return a vector (if there are no columns defined),
       ;; or we will return an empty record Structure (if there are columns defined).
       (if (= importingSW true)
           (begin
              ;; Never import more than the maximum number of records
              (if (>= rowOrMember maxRecordLimit) (error "TooManyRecs"))
              (if (= colVector #void)
                  (return (new Vector: 0))
                  (return (copy recordStructure))))
           ) ;; end importing if
       ;; If we are not importing, we will return a record (for numeric indices), or
       ;; we will return a member of this Lambdas own persistent variables (for symbolic indices).
       (if (isNumber rowOrMember) (refRow rowOrMember) (myself)[Pv:][rowOrMember])) ;; Allows Lambda polymorphism.
    (defun ref2(col row)
        vars:(record value)
        ;; Retrieve block
        (setq record (refRow row))
        (if (= record #void)
            (setq value #void)
            (setq value record[col]))
        value) ;; end of ref2
    (defun reset()
        ;; Reset the current block.
        (setq blockKEY #void)
        (setq currentBlock #void)
        (setq recordNUM 0)
        recordCount) ;; end of reset
    (defun restart()
        ;; Reset the record and block count to zero.
        (setq blockKEY #void)
        (setq currentBlock #void)
        (setq recordNUM 0)
        (setq recordCount 0)
        (setq myIndex._recordCount recordCount)
        (setq myIndex._blockCount 0)
        recordCount) ;; end of reset
    (defun restore()
        (abortTransaction myOR) 
        (copyFile backupPathName tablePathName)
        (beginTransaction myOR)
        (setq recordCount myIndex._recordCount)
        (setq recordStructure myIndex._recordStructure)
	    (if (<> myIndex._blockSize #void) (setq blockSize myIndex._blockSize))
        (commitTransaction myOR)
        recordCount) ;; end restore
    (defun runCriteria(selectCriteria)
        ;; Run the specified selection criteria.
        vars:(algorithm)
        (setq algorithm (queryBuilder selectCriteria))
        (algorithm self)
        recordCount) ;; end runCriteria
    (defun runProdSelection(selectName)
        vars:(selectSpec algorithm)
        ;; Get the specified selection criteria specification.
        (prodCriteriaLambda)
        (setq selectSpec (prodCriteriaLambda.Pv.getCriteria selectName))
        (setq algorithm (queryBuilder selectSpec))
        (algorithm self)
        recordCount) ;; end runProdSelection
    (defun runSelection(selectName)
        vars:(selectSpec algorithm)
        ;; Get the specified selection criteria specification.
        (selectCriteriaLambda)
        (setq selectSpec (selectCriteriaLambda.Pv.getCriteria selectName))
        (setq algorithm (queryBuilder selectSpec))
        (algorithm self)
        recordCount) ;; end runSelection
    (defun setColKeyness(col keyness) (error "NoAccess"))
    (defun setColName(col name)
        (if (<> myIndex #void) 
            (setq myIndex._colVector[col] name)
            (begin
                (beginTransaction myOR) 
                (setq myIndex._colVector[col] name)
                (abortTransaction myOR)))
        name) ;; end of setColName
    (defun setColType(col type) (error "NoAccess"))
    (defun setRowSeq(row record)
        vars:(newBlockKEY)
        ;; Update the record count.
        (setq recordCount (max (add1 row) recordCount))
        ;; Compute new block and record numbers.
        (setq newBlockKEY (divi row blockSize))
        (setq recordNUM (modi row blockSize))
        ;; Load new block of records?
        (if (or (= row 0)
                (<> newBlockKEY blockKEY)
                (= currentBlock #void))
            (begin
               (setq blockKEY (divi row blockSize))
               (setq currentBlock myOR[blockKEY])
               )) ;; end if
        (if (= currentBlock #void)
            (setq currentBlock (new Vector: object: 1)))
        ;; Save record into current block.
        (setq currentBlock[recordNUM] (objectToDictionary record))
        ;; Save current block?
        (if (<> blockKEY (divi (addi row 1) blockSize))
            (begin
               (commitTransaction myOR)
               (setq myOR[blockKEY] currentBlock)
               (beginTransaction myOR)
               (setq myIndex._recordCount recordCount)
               (setq myIndex._blockCount (add1 blockKEY))))
        true) ;; end of setRowSeq
    (defun setup(aName chkName backName vldFields fixUp)
       (if (<> myIndex #void)
           (begin
              (setq name aName)
              (setq checkPointPathName chkName)
              (setq backupPathName backName)
              (setq validFields vldFields)
              (setq myIndex._validFields validFields)
              (setq importFixup fixUp)
              (setq myIndex._importFixup importFixup)
              (setq myIndex._name name)
              ) ;; end then
           (begin
              (beginTransaction myOR)
              (setq name aName)
              (setq checkPointPathName chkName)
              (setq backupPathName backName)
              (setq validFields vldFields)
              (setq myIndex._validFields validFields)
              (setq importFixup fixUp)
              (setq myIndex._importFixup importFixup)
              (setq myIndex._name name)
              (commitTransaction myOR)
              )) ;; end if
           ) ;; end setup
    (defun set2(col row newValue) (error "NoAccess"))
    (defun switchUser()
        (abortTransaction myOR) 
        (beginTransaction myOR)
        (setq recordCount myIndex._recordCount)
        (setq recordStructure myIndex._recordStructure)
	(if (<> myIndex._blockSize #void) (setq blockSize myIndex._blockSize))
        (setq myIndex._blockSize blockSize)
	(setq tablePathName myOR[member: name:])
        (abortTransaction myOR)
        ) ;; end restore
    (defun truncate(rowLimit)
        vars:(newBlockKEY blockCount blockIndex lastBlock)
        ;; Are we already truncated?
        (if (>= rowLimit recordCount) (return recordCount))
        ;; Compute new block and record numbers.
        (setq newBlockKEY (divi (sub1 rowLimit) blockSize))
        (setq recordNUM (modi (sub1 rowLimit) blockSize))
        ;; Erase unnecessary blocks from the table repository.
        (beginTransaction myOR)
        (setq blockCount myIndex._blockCount)
        (loop for blockIndex from (add1 newBlockKEY) until blockCount  do
            (setq myOR[blockIndex] #void)
            ) ;; end loop
        (setq lastBlock myOR[newBlockKEY])
        (resize lastBlock (add1 recordNUM))
        (setq myOR[newBlockKEY] lastBlock)
        (setq recordCount rowLimit)
        (setq myIndex._recordCount rowLimit)
        (setq myIndex._blockCount (add1 newBlockKEY))
        (commitTransaction myOR)
        recordCount) ;; end of truncate
    ;; Initialize the Lambda when called by new ObjectRepository: or attachLibrarian. 
    ;; This function always reattaches itself to the Repository (if necessary),
    ;; and always reestablishes the table block count, record count, and other statistics.
    Continue::
    (setq self (myself))
    (setq myResident false)
    ;; Ressign the internal object repository properly.
    (cond ((= (type objRepository) Lambda:)
           (setq myOR objRepository.myOR))
          ((<> (type objRepository) ObjectRepository:)
           (error "tMgr:myOR"))
	  ((<> myOR objRepository)
           (setq myOR objRepository))
	   ) ;; end cond
    (if (<> (type myOR) ObjectRepository:) (error "tMgr:myOR"))
    ;; Reattach self as the repository librarian properly.
    (if (<> (refLibrarian myOR) self)
	(begin
        (attachLibrarian myOR self)
        (return self)
	)) ;; end if
	;; Reestablish the table block count, record count, and other statistics.
    (if (<> myIndex #void)
        (begin
           (setq recordStructure myIndex._recordStructure)
           (setq recordCount myIndex._recordCount)
           (setq validFields myIndex._validFields)
           (setq importFixup myIndex._importFixup)
           (setq name myIndex._name)
           (if (<> myIndex._blockSize #void) (setq blockSize myIndex._blockSize))
           (setq myIndex._blockSize blockSize)
           (setq tablePathName myOR[member: name:])
           ) ;; end then
        (begin
           (beginTransaction myOR)
           (setq recordStructure myIndex._recordStructure)
           (setq recordCount myIndex._recordCount)
           (setq validFields myIndex._validFields)
           (setq importFixup myIndex._importFixup)
           (setq name myIndex._name)
           (if (<> myIndex._blockSize #void) (setq blockSize myIndex._blockSize))
           (setq myIndex._blockSize blockSize)
           (setq tablePathName myOR[member: name:])
           (commitTransaction myOR)
           )) ;; end if
    self) ;; end of tableDbms





















;;**EXPORTKEY**:tableDbms:_makeBlockList
(defChildLambda tableDbms:_makeBlockList(runStart runLength runEnd)
;; *******************************************************************
;; summary:  Return a vector containing the run sequence starting
;;           with the specified runStart and running for the
;;           specified runLength, but not to exceed the specified
;;           runEnd.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     runStart   The starting integer of the run.
;;           runCount   The number of integers in the run.
;;           runEnd     The maximum integer in the run.
;; Return:   runVector  The run vector or #void.        
;; *******************************************************************
    vars:(runIndex             ;; Index of run integers
          runVector            ;; Result run vector
          ) ;; end temporary variables
    ;; Compute the number of integers in this run.
    ;; Note: We cannot exceed the specified runEnd.
    (if (>= (+ runStart runLength) runEnd)
        (setq runLength (- runEnd runStart)))
    (if (<= runLength 0)
        (return runVector))
    ;; Create the run vector and fill it with sequential integers.
    (setq runVector (makeVector runLength))
    (setq runStart (integer runStart))
    (loop for runIndex from 0 until runLength do
        (setq runVector[runIndex] runStart)
        (setq runStart (addi runStart 1))
        ) ;; end fill loop
    runVector) ;; end of _makeBlockList





















;;**EXPORTKEY**:tableDbms:_mergeBlockList
(defChildLambda tableDbms:_mergeBlockList(blockListOne blockListTwo sortLambda)
;; *******************************************************************
;; summary:  Merge two lists of blocks into a sequential run of blocks. 
;;           Each blockList is a vector of block keys. Each blockList
;;           represents a sequential run of block keys, and the keys
;;           in blockListTwo are immediately contiguous to blockListOne.
;;           The records in blockListOne are presorted. The records in 
;;           blockListTwo are presorted. When the merge is complete,
;;           the records will be sorted across both blockLists.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     blockListOne   A vector of block keys to be merged.
;;           blockListTwo   A vector of block keys to be merged.
;;           sortLambda      Sort lambda value for record pairs.
;; Return:   true        
;; *******************************************************************
    vars:(blockListOut         ;; Block list for merged output
          outBlock             ;; Block for merged output
          outKey               ;; Block key for merged output
          recordOne            ;; Record one for merge
          recordTwo            ;; Record two for merge
          ) ;; end temporary variables
    ;; Return if either block list is empty.
    (if (or (= blockListOne #void) 
            (<= (length blockListOne) 0)
            (= blockListTwo #void) 
            (<= (length blockListTwo) 0)) 
        (return #void))
    ;; Begin the merge by reading the first two records.
    ;; Note: We create a list of output block keys,
    ;;       then we rename the input block keys so that
    ;;       we will not overwrite them with our output.
    (setq blockListOut (append blockListOne blockListTwo))
    (setq outBlock (^new Vector: object: 0))
    (beginTransaction myOR)
    (_renameBlocks blockListOne myIndex)
    (_renameBlocks blockListTwo myIndex)
    (setq recordOne (_readBlockList blockListOne true))
    (setq recordTwo (_readBlockList blockListTwo true))
    (commitTransaction myOR)
    ;; Merge the records in both block lists together
    ;; into a single contiguous presorted run of blocks.
    (while (or (<> recordOne #void) (<> recordTwo #void)) do
        (cond
           ;; Manage the case where the second list is empty.
           ((= recordTwo #void)
            (begin
               (setq outBlock[(length outBlock)] recordOne)
               (setq recordOne (_readBlockList blockListOne true))
               )) ;; end second list is empty.
           ;; Manage the case where the first list is empty.
           ((= recordOne #void)
            (begin
               (setq outBlock[(length outBlock)] recordTwo)
               (setq recordTwo (_readBlockList blockListTwo true))
               )) ;; end first list is empty
           ;; Manage the case where record one should be output.
           ((sortLambda recordOne recordTwo)
            (begin
               (setq outBlock[(length outBlock)] recordOne)
               (setq recordOne (_readBlockList blockListOne true))
               )) ;; end record one comes first
           ;; Manage the case where record two should be output.
           (else
            (begin
               (setq outBlock[(length outBlock)] recordTwo)
               (setq recordTwo (_readBlockList blockListTwo true))
               )) ;; end record two comes first
            ) ;; end cond
        ;; Write the output block (if it is full).
        ;; Note: The output key is taken off the front
        ;;       of the list of output block keys. This
        ;;       means that the merged blocks are written
        ;;       back to the database using original keys.
        (if (>= (length outBlock) blockSize)
            (begin
               (setq outKey blockListOut[0])
               (delete blockListOut 0) 
               (setq myOR[outKey] outBlock)
               (setq outBlock (^new Vector: object: 0))
               )) ;; end if
        ) ;; end while
    ;; Write the output block (if it is not empty).
    ;; Note: The output key is taken off the front
    ;;       of the list of output block keys. This
    ;;       means that the merged blocks are written
    ;;       back to the database using original keys.
    (if (> (length outBlock) 0)
        (begin
           (setq outKey blockListOut[0])
           (delete blockListOut 0) 
           (setq myOR[outKey] outBlock)
           (setq outBlock #void)
           )) ;; end if
    ;; Always return true.
    true) ;; end of _mergeBlockList






















;;**EXPORTKEY**:tableDbms:_readBlockList
(defChildLambda tableDbms:_readBlockList(blockList eraseSW)
;; *******************************************************************
;; summary:  Read the next record from the specified list of blocks. 
;;           The blockList is a vector of block keys. As a new block
;;           is read, it is stored in the cdr of the blockList. As
;;           a block is exhausted, it is erased from the repository,
;;           and the blockList is altered to reflect the erased block.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     blockList   A vector of block keys to be read.
;;           eraseSW     True if blocks are to be erased after reading.
;; Return:   theRecord   The next database record (or #void).       
;; *******************************************************************
    vars:(blockKey theBlock theRecord) 
    ;; Return #void if there are no more records.
    (if (and (= (cdr blockList) #void) (<= (length blockList) 0)) (return #void))
    ;; Make sure the current block is loaded from the repository.
    ;; Note: If we must load a new block, then we
    ;;       erase the block from the repository
    ;;       and from the blockList after we load.
    (if (= (cdr blockList) #void)
        (begin
           (setq blockKey blockList[0])
           (setCdr blockList myOR[blockKey])
           (if (= eraseSW true)
               (setq myOR[blockKey] #void))
           (delete blockList 0)
           )) ;; end if
    ;; Read the next record from the current block.
    ;; Note: If we exhaust the current block, then we
    ;;       set the cdr of the blockList to #void.
    (setq theBlock (cdr blockList))
    (if (or (= theBlock #void) (<= (length theBlock) 0)) (return #void))
    (setq theRecord theBlock[0])
    (delete theBlock 0)
    (if (<= (length theBlock) 0) (setCdr blockList #void))
    ;; Return the next record.
    theRecord) ;; end of _readBlockList





















;;**EXPORTKEY**:tableDbms:_renameBlocks
(defChildLambda tableDbms:_renameBlocks(blockList anIndex)
;; *******************************************************************
;; summary:  Rename the specified list of blocks. The block keys are
;;           positive integers. Each key is renamed to its negation
;;           and restored in the specified repository index. The
;;           blockList is altered to reflect the new keys.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     blockList   A vector of block keys to be renamed.
;;           keyIndex    A Directory of block keys.
;; Return:   true        
;; *******************************************************************
    vars:(blockCount blockIndex oldKey newKey oldValue) 
    ;; Rename each block to its negation.
    ;; Note: This does not reposition the blocks  
    ;;       in the table database. The blocks
    ;;       are only renamed in the index.
    (setq blockCount (length blockList))
    (loop for blockIndex from 0 until blockCount do
        (setq oldKey blockList[blockIndex])
        (setq newKey (- oldKey))
        (setq blockList[blockIndex] newKey)
        (setq oldValue anIndex[oldKey])
        (setq anIndex[oldKey] #void)
        (setq anIndex[newKey] oldValue)
        ) ;; end of loop
    true) ;; end of _renameBlocks





















;;**EXPORTKEY**:tableDbms:copyDatabase
(defChildLambda tableDbms:copyDatabase(fileTwo) 
;; *******************************************************************
;; summary:  Copy the entire contents of this table database to table
;;           database two (with file defragmentation).
;; Args :    fileTwo    The name of the table database file to receive the copy. 
;; Return:   true       If no error occurs.
;; *******************************************************************
   vars:(fileTwo             ;; The file id for file two.
         databaseTwo         ;; The Object Repository for file two.
         dataBlock           ;; The block of data to be copied.
         blockKey            ;; The key for the block of data to be copied.
         blockIndex          ;; The number of bytes already copied.
         indexLength         ;; The number of records to be copied.
         ) ;; end of temporary variables
   ;; Open and clear database file two for receiving the copied records.
   (setq databaseTwo (^new ObjectRepository: fileTwo clear:))
   (setq databaseTwo (dummyDbms databaseTwo))
   (beginTransaction myOR)
   (beginTransaction databaseTwo)
   (setq indexLength (length myIndex))
   ;(writeln "") 
   ;(writeln "Starting copy of " indexLength " blocks to " fileTwo) 
   ;; Copy each record from this table database to table database two.
   (loop for blockIndex from 0 until indexLength do
      (setq blockKey myIndex[blockIndex 0])
      ;(writeln "Copying block [" blockIndex "] key = {" blockKey "}") 
      (if (isSymbol blockKey)
          (setq databaseTwo.myIndex[blockKey] myIndex[blockIndex 1])
          (setq databaseTwo.myOR[blockKey] myOR[blockKey])) 
      ) ;; end of loop
   ;; Close both database files.
   ;(writeln "Ending copy of " indexLength " blocks to " fileTwo) 
   (abortTransaction myOR)
   (commitTransaction databaseTwo)
   true) ;; end of copyDatabase
























;;**EXPORTKEY**:tableDbms:copyFile
(defChildLambda tableDbms:copyFile(fileOne fileTwo) 
;; *******************************************************************
;; summary:  Copy the entire contents of file one to file two.
;; Args :    fileOne    The name of the file to be copied. 
;;           fileTwo    The name of the file to receive the copy. 
;; Return:   The number of bytes copied.
;; *******************************************************************
   vars:(fileOneID           ;; The file id for file one.
         fileTwoID           ;; The file id for file two.
         dataBlock           ;; The block of data to be copied.
         (fileType 0)        ;; The file type for standard files.
         (byteCount 0)       ;; The number of bytes to be copied.
         byteIndex           ;; The number of bytes already copied.
         (byteLength 100000) ;; The number of bytes to be read.
         ) ;; end of temporary variables
   ;; Open file one for reading and file two for writing.
   (setq fileOneID (fileOpen fileOne 0 fileType))
   (setq fileTwoID (fileOpen fileTwo 1 fileType))
   ;; Find the length of file one and reset file pointer.
   (setq byteCount (fileSeek fileOneID 0 2))
   (fileSeek fileOneID 0 1)
   ;; Read and write blocks until there is no more data
   (loop for byteIndex from 0 until byteCount by 100000 do
      ;; Make sure we read only to the end of the file.
      (if (> (+ byteIndex byteLength) byteCount)
          (setq byteLength (- byteCount byteIndex))
          ) ;; end if
      ;; Copy a block from file one to file two.
      (setq dataBlock (fileRead fileOneID byteLength))
      (fileWrite fileTwoID dataBlock)
      ) ;; end of loop
   ;; Close both files.
   (fileClose fileOneID 1)
   (fileClose fileTwoID 1)
   byteCount) ;; end of copyFile





















;;**EXPORTKEY**:tableDbms:delimitedCells
(defChildLambda tableDbms:delimitedCells(startRow endRow startCol endCol)
;; *******************************************************************
;; summary:  Return a set of values from the table database as a
;;           tab delimited string.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     startRow    Starting row for values
;;           endRow      Ending row for values
;;           startCol    Starting column for values
;;           endCol      Ending column for values    
;; Return:   String      A string with field values separated by tabs
;;                       and rows separated by the contents of _eol.
;; *******************************************************************
    vars:(i getRow getCol dls cellValue realRow 
          recordBLOCK startBlockKey endBlockKey
          startRecord endRecord record tab) 
    ;; Retrieve the start and end record blocks.
    ;; Note: We assume that no more than two 
    ;;       blocks will ever be necessary.
    (if (<> myIndex #void)
        (begin 
            (setq startBlockKey (divi startRow blockSize))
            (setq recordBLOCK myOR[startBlockKey])
            (setq endBlockKey (divi endRow blockSize))
            (if (<> startBlockKey endBlockKey)
                (setq recordBLOCK (append recordBLOCK myOR[endBlockKey])))
                ) ;; end of then
        (begin
            (beginTransaction myOR) 
            (setq startBlockKey (divi startRow blockSize))
            (setq recordBLOCK myOR[startBlockKey])
            (setq endBlockKey (divi endRow blockSize))
            (if (<> startBlockKey endBlockKey)
                (setq recordBLOCK (append recordBLOCK myOR[endBlockKey])))
            (abortTransaction myOR))
            ) ;; end of else
    ;; Compute blocked record numbers.
    (setq startRecord (modi startRow blockSize))
    (setq endRecord (addi startRecord (subi endRow startRow)))
    ;; Return a tab delimited string of cell values for
    ;; all cells between the following rows and columns.
    (setq dls "")
    (setq tab (append "" (char 09)))
    (setq realRow startRow)
    (loop for getRow from startRecord to endRecord do
       (setq dls (append dls realRow (char 09)))
       (++ realRow)
       (setq record (objectToStructure (copy recordStructure) recordBLOCK[getRow]))
       (loop for getCol from startCol until endCol do
            (setq cellValue record[getCol])
            (if (= cellValue #void) (setq cellValue ""))
            (setq cellValue (append "" cellValue)) 
            (setq cellValue (substitute cellValue (char 09)  " "))
            (setq cellValue (substitute cellValue (char 13)  " "))
            (setq dls (append dls cellValue (char 09)))
            ) ;;end getCol loop
        (setq dls (append dls (char 10) (char 13)))
        ) ;;end getRow loop
    (append dls "")) ;; end of delimitedCells





















;;**EXPORTKEY**:tableDbms:exportCells
(defChildLambda tableDbms:exportCells(formatName exportAs exportDelim fileName startRow endRow)
;; *******************************************************************
;; summary:  Return a set of values from the table database as a
;;           tab delimited string.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     formatName  type of format to export
;;           exportAs    export as "delimited","fixed",etc. 
;;           startRow    Starting row for values
;;           endRow      Ending row for values
;; Return:   String      A string with field values separated by tabs
;;                       and rows separated by the contents of _eol.
;; *******************************************************************
    vars:(
          cellValue               ;; Data field value from database
          dls                     ;; Row output buffer
          exportFormatRecord      ;; Export record layout format data
          fieldName               ;; Name of output field
          fieldLen                ;; Field output length
          fileId                  ;; File handle
          getRow                  ;; Row index
          layoutLength            ;; Length of layout data
          myPos                   ;; Delimiter position terminating field in layout data
          outputField             ;; Buffer for building fixed-length output field
          record                  ;; Record data
         ) 

    ;; Format exportAs parameter to avoid confusion
    (setq exportAs (upcase (left exportAs 3)))

    ;; Retrieve the export layout from the exportLayoutLambda.
    (if (= (setq exportFormatRecord (exportLayoutLambda.getLayoutRecord formatName)) #void)
        (error "unknownFormat")
    )

    ;; Return a tab delimited string of cell values for
    ;; all cells between the following rows and columns.

    ;; Initialize row buffer
    (setq dls "")
     
    ;; For all rows by index, assuming user gives us 1-based record parameters
    ;; and further assuming that refRowseq is zero-based
    (if (<= startRow 0) (setq startRow 1))
    (if (> startRow endRow) (setq endRow startRow))
    (if (>= endRow recordCount) (setq endRow recordCount))

    ;; Open file, getting handle
    (setq fileId (fileOpen  fileName 1 0))     
    (updateBegin)

    (loop for getRow from (sub1 startRow) until endRow do
         
       ;; Read database row
       (setq record (updateRead getRow))
                  
       ;; Initialize position variables
       (setq myPos 0)
       (setq layoutLength (length exportFormatRecord.fieldList))
                 
       ;; Output fields for this row
       (while (< myPos layoutLength) do
          ;; Parse out the export field name
          (setq fieldName exportFormatRecord.fieldList[myPos])
                                        
          ;; Store the field's value
          (setq cellValue record[(symbol fieldName)])
                 
          ;; Set #void values to an empty string -- 0 bytes will be output
          (if (= cellValue #void) then (setq cellValue ""))
                  
          ;; Format numeric field values
          (if (= (isString cellValue) false)
              (setq cellValue (string cellValue)))
                
          ;; Branch to fixed-length format -------------------------------
          (if (= exportAs "FIX") 
              (begin
                
                 ;; Store the length of the output field
                 (setq fieldLen exportFormatRecord.fieldData[exportFormatRecord.fieldList[myPos]])
                       
                 ;; Initialize the outputField to spaces
                 (setq outputField (append cellValue (rept " " fieldLen)))
                        
                 ;; Truncate any excess bytes per the specified field len
                 (setq outputField (left outputField fieldLen))
              
                 ;; Add the output field to the row buffer
                 (setq dls (append dls outputField))
              )
          )
          
          ;; Branch to Delimited ------------------------------------
          (if (= exportAs "DEL")
              (begin
                (setq dls (append dls cellValue))
                (if (< (add1 myPos) layoutLength)
                    (setq dls (append dls (char (number exportDelim)))))
              )
          )

          ;; Increment position variable
          (++ myPos)

       ) ;; end while (< layoutLength myPos)
       
       ;; Output row to file               
       (fileWrite  fileId  (append dls _eol))
       ;; Clear row buffer
       (setq dls "") 
       ;; process next row
    ) ;;end getRow loop

    ;; Close the file
    (fileClose  fileId  1)
    (updateEnd)

    true) ;; end of exportCells



















;;**EXPORTKEY**:tableDbms:fieldKeys
(defChildLambda tableDbms:fieldKeys(fieldName)
;; *******************************************************************
;; summary:  Return a set of field key values from the table database
;;           as a tab delimited string.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     fieldName   Name of the Dictionary field whose valid keys 
;;                       are to be returned.  
;; Return:   String      A string with field key values separated by 
;;                       tabs and rows separated by the contents of _eol.
;; *******************************************************************
    vars:(i dls typeValue record) 
    ;; Return a tab delimited string of field key values.
    (setq dls "")
    ;; Set the field key values
    (cond
        ((= fieldName "DepProducts")
         (setq record #(CHKC CHKB INTC INTB SAVC SAVB CDC CDB IRA)))
        ((= fieldName "CrdProducts")
         (setq record #(CCVC CCMS CCMG CCVP CCVB CCMB CCVR)))
        ((= fieldName "LinProducts")
         (setq record #(CLE CLC PLI HLIV EMXC EMXB ACLB DCLB PDLB OCLB)))
        ((= fieldName "LonProducts")
         (setq record #(ADIR ADLR RV MFH PLO CBLO HLOF HLOV FRLC FRLB OLON REFC REVC REFB REVB)))
        (else
         (setq record #void))
        ) ;; end cond
    (setq endCol (length record))
    (loop for i from 0 until endCol do
        (setq typeValue record[i])
        (if (= typeValue #void) then (setq typeValue ""))
        (setq dls (append dls typeValue (char 09)))
        ) ;;end field type loop
    (setq dls (append dls (char 10) (char 13)))
    (append dls "")) ;; end of fieldKeys





















;;**EXPORTKEY**:tableDbms:fieldSubfields
(defChildLambda tableDbms:fieldSubfields(fieldName)
;; *******************************************************************
;; summary:  Return a set of field subfield name values from the table 
;;           database as a tab delimited string.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     fieldName   Name of the Dictionary field whose valid subfield 
;;                       names are to be returned.  
;; Return:   String      A string with field subfield name values separated by 
;;                       tabs and rows separated by the contents of _eol.
;; *******************************************************************
    vars:(i dls typeValue record) 
    ;; Return a tab delimited string of field key values.
    (setq dls "")
    ;; Set the field key values
    (cond
        ((= fieldName "DepProducts") (setq record _depRollupFields))
        ((= fieldName "DepAccounts") (setq record _depDetailFields))
        ((= fieldName "CrdProducts") (setq record _cardRollupFields))
        ((= fieldName "CrdAccounts") (setq record _cardDetailFields))
        ((= fieldName "LinProducts") (setq record _lineRollupFields))
        ((= fieldName "LinAccounts") (setq record _lineDetailFields))
        ((= fieldName "LonProducts") (setq record _loanRollupFields))
        ((= fieldName "LonAccounts") (setq record _loanDetailFields))
        (else
         (setq record #void))
        ) ;; end cond
    (setq endCol (length record))
    (loop for i from 0 until endCol do
        (setq typeValue record[i 0])
        (if (= typeValue #void) then (setq typeValue ""))
        (setq dls (append dls typeValue (char 09)))
        ) ;;end field type loop
    (setq dls (append dls (char 10) (char 13)))
    (append dls "")) ;; end of fieldSubfields





















;;**EXPORTKEY**:tableDbms:fieldSubtypes
(defChildLambda tableDbms:fieldSubtypes(fieldName)
;; *******************************************************************
;; summary:  Return a set of field subfield type values from the table 
;;           database as a tab delimited string.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     fieldName   Name of the Dictionary field whose valid subfield 
;;                       types are to be returned.  
;; Return:   String      A string with field subfield type values separated by 
;;                       tabs and rows separated by the contents of _eol.
;; *******************************************************************
    vars:(i dls typeValue record) 
    ;; Return a tab delimited string of field key values.
    (setq dls "")
    ;; Set the field key values
    (cond
        ((= fieldName "DepProducts") (setq record _depRollupFields))
        ((= fieldName "DepAccounts") (setq record _depDetailFields))
        ((= fieldName "CrdProducts") (setq record _cardRollupFields))
        ((= fieldName "CrdAccounts") (setq record _cardDetailFields))
        ((= fieldName "LinProducts") (setq record _lineRollupFields))
        ((= fieldName "LinAccounts") (setq record _lineDetailFields))
        ((= fieldName "LonProducts") (setq record _loanRollupFields))
        ((= fieldName "LonAccounts") (setq record _loanDetailFields))
        (else
         (setq record #void))
        ) ;; end cond
    (setq endCol (length record))
    (loop for i from 0 until endCol do
        (setq typeValue record[i 1])
        (if (= typeValue #void) then (setq typeValue ""))
        (setq dls (append dls typeValue (char 09)))
        ) ;;end field type loop
    (setq dls (append dls (char 10) (char 13)))
    (append dls "")) ;; end of fieldSubtypes





















;;**EXPORTKEY**:tableDbms:fieldTypes
(defChildLambda tableDbms:fieldTypes()
;; *******************************************************************
;; summary:  Return a set of field type values from the table database
;;           as a tab delimited string.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     fieldName   
;; Return:   String      A string with field type values separated by 
;;                       tabs and rows separated by the contents of _eol.
;; *******************************************************************
    vars:(i dls typeValue record) 
    ;; Return a tab delimited string of field type values.
    (setq dls "")
    (setq record validFields)
    (setq endCol (length record))
    (loop for i from 0 until endCol do
        (setq typeValue record[i])
        (if (= typeValue #void) then (setq typeValue ""))
        (setq dls (append dls typeValue (char 09)))
        ) ;;end field type loop
    (setq dls (append dls (char 10) (char 13)))
    (append dls "")) ;; end of fieldTypes





















;;**EXPORTKEY**:tableDbms:importLegacyData
(defChildLambda tableDbms:importLegacyData(...)
;; *******************************************************************
;; summary:  Import a time slice into the table database.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     tableDate      (optional) The date of this extract    
;;           sbfFormatSW    (optional) The boolean format switch 
;;                                     (true if NO separate column 
;;                                      headers file)   
;;           colFile        (optional) The path and file name of the 
;;                                     separate column headers file   
;;           tableFile      (optional) The path and file name of the 
;;                                     tab delimited table data file
;;           maxRecordLimit (optional) The maximum number of records 
;;                                     to import.
;; Note:     If only one argument is passded, it is treated as the 
;;           error return passed by onError.
;; Return:   true
;; *******************************************************************
    vars:(i j n commands colHeaders 
          tableDate colFile tableFile
          recordLimit startTime endTime
          ) ;; end of temporary variables
    (writeln "Starting record import." )
    (setq importNilRows 0)
    ;; Is this an error during table extract import?
    (if (= (argCount) 1) (goto ErrorContinue:))
    ;; Request the date for this table extract import.
    (if (>= (argCount) 1)
        (setq tableDate (argFetch 0))
        (begin
           (setq tableDate #void)
           (setq tableDate (input "Enter table extract date." (string (date (integer (now))))))
           (if (isBoolean tableDate) (return true))
           (setq tableDate (date tableDate))
           (setq importDate tableDate)
           )) ;; end if
    ;; Determine the column header location and file name.
    (if (>= (argCount) 3)
        (begin
           (setq sbfFormatSW (argFetch 1))
           (setq colFile (argFetch 2)))
        (begin
           ;; Request column header options.
           (setq colFile #void)
           (setq commands #("Header with data" "Header apart from data"))
           (setq n (select  "tableDataLambda"  "Where are the column headers?" commands 0))
           (if (isBoolean n) (return false))
           (if (= n 0)
               (setq sbFormatSW true)
               (setq sbfFormatSW false))
           )) ;; end outter if
    ;; Request the path and file for this table extract import.
    (if (>= (argCount) 4)
        (setq tableFile (argFetch 3))
        (begin
           (setq tableFile (input "Enter table path and file name." ""))
           (if (isBoolean tableFile) (return true))
           )) ;; end if
    ;; Request the import record limit for this table extract import.
    (if (>= (argCount) 5)
        (setq maxRecordLimit (argFetch 4))
        (setq maxRecordLimit 1000000000)
        ) ;; end if
    ;; Do we need to open a separate column header file?
    (setq importFixupSW false)
    (if (= sbfFormatSW false)
        (begin
           (if (= colFile "default")
               (begin
                 (setq colHeaders (cnvColHeaders))
                 (setq importFixupSW true))
               (begin 
                 (setq commands (readTextFile colFile))
                 (if (= commands false) (return false))
                 (setq colHeaders (stringToVector commands _eol))
                 (setq commands #void) ;; conserve space
                 (setq n (length colHeaders))
                 (loop for i from 0 until n do
                     (setq j (find #\tab colHeaders[i]))
                     (if (isNumber j) 
                         (setq colHeaders[i] (left colHeaders[i] j)))         
                     ))) ;; end of for loop
           (setq colVector colHeaders)
           (setq colHeaders #void)
           ) ;; end then
         (begin
           (setq colVector #void)
           (setq colHeaders #void)
           ) ;; end else
         ) ;; end of inner if
    ;; Make sure the record template is constructed correctly.
    (if (<> colVector #void)
        (setq recordStructure (objectToStructure colVector #(#void)))
        ) ;; end build record template
    ;; Make sure the user opens the file correctly.
    (setq importFileID (fileOpen tableFile 0 0))
    (if (= importFileID false) (return false))
    ;; Destroy all the records, statistics, and 
    ;; indices in the existing database.
    (setq importingSW false)
    (clear myOR)
    ;; Load myIndex and reset all database statistics.
    (beginTransaction myOR)
    (setq myIndex._colVector #void)
    (setq myIndex._recordCount 0)
    (setq recordCount 0)
    (setq myIndex._blockCount 0)
    (setq myIndex._tableDate tableDate)
    ;; Reset all import related persistent variables.
    (setq importingSW true)
    (setq blockKEY 0)
    (setq currentBlock #void)
    ;; Set up import timing statistics
    (setq startTime (getTickCount 0))
    ;; Import tab delimited file and close file when done.
    ;; Note: This will recursively call setColName, set1, etc.
    (onError (myself))
    (^importTab importFileID self recordVectors:)
    ;; Return here if an import error occurs
    ErrorContinue::
    (if (isNumber importFileID)
        (fileClose importFileID 1))
    (setq importFileID #void)
    ;; Make sure we write out the last block when done.
    (if (<> currentBlock #void)
        (begin
            (setq myOR[blockKEY] currentBlock)
            (setq myIndex._blockCount (addi blockKEY 1))
            (setq blockKEY 0)
            (setq currentBlock #void)))
    ;; Complete the import process.
    (setq endTime (integer (getTickCount startTime)))
    (writeln "Records imported = " recordCount " in " endTime " Seconds" )
    (commitTransaction myOR)
    (setq importingSW false)
    ;; Copy the freshly imported table database file
    ;; to the backup file for use by the findAll method.
    (copyFile tablePathName backupPathName)
    true) ;; end tableDbms:ImportTableDbmsData





















;;**EXPORTKEY**:tableDbms:importTab
(defChildLambda tableDbms:importTab(...)
;; *******************************************************************
;; summary:  Import a time slice into the table database.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     tableDate      (optional) The date of this extract    
;;           sbfFormatSW    (optional) The boolean format switch 
;;                                     (true if NO separate column 
;;                                      headers file)   
;;           colFile        (optional) The path and file name of the 
;;                                     separate column headers file   
;;           tableFile      (optional) The path and file name of the 
;;                                     tab delimited table data file
;;           maxRecordLimit (optional) The maximum number of records 
;;                                     to import.
;;           fixupSW        (optional) True if imported records are 
;;                                     to use default fixup function.
;; Note:     If only one argument is passded, it is treated as the 
;;           error return passed by onError.
;; Return:   true
;; *******************************************************************
    vars:(i j n commands colHeaders 
          tableDate colFile tableFile
          recordLimit startTime endTime
          fixupSW
          ) ;; end of temporary variables
    (setq importNilRows 0)
    ;; Is this an error during table extract import?
    (if (= (argCount) 1) (goto ErrorContinue:))
    ;; Request the date for this table extract import.
    (if (>= (argCount) 1)
        (setq tableDate (argFetch 0))
        (begin
           (setq tableDate #void)
           (setq tableDate (input "Enter table extract date." (string (date (integer (now))))))
           (if (isBoolean tableDate) (return true))
           (setq tableDate (date tableDate))
           (setq importDate tableDate)
           )) ;; end if
    ;; Determine the column header location and file name.
    (if (>= (argCount) 3)
        (begin
           (setq sbfFormatSW (argFetch 1))
           (setq colFile (argFetch 2)))
        (begin
           ;; Request column header options.
           (setq colFile #void)
           (setq commands #("Header with data" "Header apart from data"))
           (setq n (select  "tableDataLambda"  "Where are the column headers?" commands 0))
           (if (isBoolean n) (return false))
           (if (= n 0)
               (setq sbFormatSW true)
               (setq sbfFormatSW false))
           )) ;; end outter if
    ;; Request the path and file for this table extract import.
    (if (>= (argCount) 4)
        (setq tableFile (argFetch 3))
        (begin
           (setq tableFile (input "Enter table path and file name." ""))
           (if (isBoolean tableFile) (return true))
           )) ;; end if
    ;; Request the import record limit for this table extract import.
    (if (>= (argCount) 5)
        (setq maxRecordLimit (argFetch 4))
        (setq maxRecordLimit 1000000000)
        ) ;; end if
    ;; Request the import fixup switch for this table extract import.
    (if (>= (argCount) 6)
        (setq fixupSW (argFetch 5))
        (setq fixupSW true)
        ) ;; end if
    ;; Do we need to open a separate column header file?
    (setq importFixupSW false)
    (if (= sbfFormatSW false)
        (begin
           (if (= colFile "default")
               (begin
                 (setq colHeaders (cnvColHeaders))
                 (setq importFixupSW fixupSW))
               (begin 
                 (setq commands (readTextFile colFile))
                 (if (= commands false) (return false))
                 (setq colHeaders (stringToVector commands _eol))
                 (setq commands #void) ;; conserve space
                 (setq n (length colHeaders))
                 (loop for i from 0 until n do
                     (setq j (find #\tab colHeaders[i]))
                     (if (isNumber j) 
                         (setq colHeaders[i] (left colHeaders[i] j)))         
                     ))) ;; end of for loop
           (setq colVector colHeaders)
           (setq colHeaders #void)
           ) ;; end then
         (begin
           (setq colVector #void)
           (setq colHeaders #void)
           ) ;; end else
         ) ;; end of inner if
    ;; Make sure the record template is constructed correctly.
    (if (<> colVector #void)
        (setq recordStructure (objectToStructure colVector #(#void)))
        ) ;; end build record template
    ;; Make sure the user opens the file correctly.
    (setq importFileID (fileOpen tableFile 0 0))
    (if (= importFileID false) (return false))
    ;; Destroy all the records, statistics, and 
    ;; indices in the existing database.
    (setq importingSW false)
    (clear myOR)
    ;; Load myIndex and reset all database statistics.
    (beginTransaction myOR)
    (setq myIndex._colVector #void)
    (setq myIndex._recordCount 0)
    (setq recordCount 0)
    (setq myIndex._blockCount 0)
    (setq myIndex._tableDate tableDate)
    ;; Reset all import related persistent variables.
    (setq importingSW true)
    (setq blockKEY 0)
    (setq currentBlock #void)
    ;; Set up import timing statistics
    (setq startTime (getTickCount 0))
    ;; Import tab delimited file and close file when done.
    ;; Note: This will recursively call setColName, set1, etc.
    (onError (myself))
    (^importTab importFileID self recordVectors:)
    ;; Return here if an import error occurs
    ErrorContinue::
    (if (isNumber importFileID)
        (fileClose importFileID 1))
    (setq importFileID #void)
    ;; Make sure we write out the last block when done.
    (if (<> currentBlock #void)
        (begin
            (setq myOR[blockKEY] currentBlock)
            (setq myIndex._blockCount (addi blockKEY 1))
            (setq blockKEY 0)
            (setq currentBlock #void)))
    ;; Complete the import process.
    (setq endTime (integer (getTickCount startTime)))
    (writeln "Records imported = " recordCount " in " endTime " Seconds" )
    (commitTransaction myOR)
    (setq importingSW false)
    ;; Copy the freshly imported table database file
    ;; to the backup file for use by the findAll method.
    (copyFile tablePathName backupPathName)
    true) ;; end tableDbms:ImportTab





















;;**EXPORTKEY**:tableDbms:makeBlockList
(defChildLambda tableDbms:makeBlockList(runStart runLength runEnd)
;; *******************************************************************
;; summary:  Return a vector containing the run sequence starting
;;           with the specified runStart and running for the
;;           specified runLength, but not to exceed the specified
;;           runEnd.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     runStart   The starting integer of the run.
;;           runCount   The number of integers in the run.
;;           runEnd     The maximum integer in the run.
;; Return:   runVector  The run vector or #void.        
;; *******************************************************************
    vars:(runIndex             ;; Index of run integers
          runVector            ;; Result run vector
          ) ;; end temporary variables
    ;; Compute the number of integers in this run.
    ;; Note: We cannot exceed the specified runEnd.
    (if (>= (+ runStart runLength) runEnd)
        (setq runLength (- runEnd runStart)))
    (if (<= runLength 0)
        (return runVector))
    ;; Create the run vector and fill it with sequential integers.
    (setq runVector (makeVector runLength))
    (setq runStart (integer runStart))
    (loop for runIndex from 0 until runLength do
        (setq runVector[runIndex] runStart)
        (setq runStart (addi runStart 1))
        ) ;; end fill loop
    runVector) ;; end of makeBlockList




















;;**EXPORTKEY**:tableDbms:mergeBlockList
(defChildLambda tableDbms:mergeBlockList(blockListOne blockListTwo sortLambda)
;; *******************************************************************
;; summary:  Merge two lists of blocks into a sequential run of blocks. 
;;           Each blockList is a vector of block keys. Each blockList
;;           represents a sequential run of block keys, and the keys
;;           in blockListTwo are immediately contiguous to blockListOne.
;;           The records in blockListOne are presorted. The records in 
;;           blockListTwo are presorted. When the merge is complete,
;;           the records will be sorted across both blockLists.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     blockListOne   A vector of block keys to be merged.
;;           blockListTwo   A vector of block keys to be merged.
;;           sortLambda      Sort lambda value for record pairs.
;; Return:   true        
;; *******************************************************************
    vars:(blockListOut         ;; Block list for merged output
          outBlock             ;; Block for merged output
          outKey               ;; Block key for merged output
          recordOne            ;; Record one for merge
          recordTwo            ;; Record two for merge
          ) ;; end temporary variables
    ;; Return if either block list is empty.
    (if (or (= blockListOne #void) 
            (<= (length blockListOne) 0)
            (= blockListTwo #void) 
            (<= (length blockListTwo) 0)) 
        (return #void))
    ;; Begin the merge by reading the first two records.
    ;; Note: We create a list of output block keys,
    ;;       then we rename the input block keys so that
    ;;       we will not overwrite them with our output.
    (setq blockListOut (append blockListOne blockListTwo))
    (setq outBlock (^new Vector: object: 0))
    (beginTransaction myOR)
    (renameBlocks blockListOne myIndex)
    (renameBlocks blockListTwo myIndex)
    (setq recordOne (readBlockList blockListOne true))
    (setq recordTwo (readBlockList blockListTwo true))
    (commitTransaction myOR)
    ;; Merge the records in both block lists together
    ;; into a single contiguous presorted run of blocks.
    (while (or (<> recordOne #void) (<> recordTwo #void)) do
        (cond
           ;; Manage the case where the second list is empty.
           ((= recordTwo #void)
            (begin
               (setq outBlock[(length outBlock)] recordOne)
               (setq recordOne (readBlockList blockListOne true))
               )) ;; end second list is empty.
           ;; Manage the case where the first list is empty.
           ((= recordOne #void)
            (begin
               (setq outBlock[(length outBlock)] recordTwo)
               (setq recordTwo (readBlockList blockListTwo true))
               )) ;; end first list is empty
           ;; Manage the case where record one should be output.
           ((sortLambda recordOne recordTwo)
            (begin
               (setq outBlock[(length outBlock)] recordOne)
               (setq recordOne (readBlockList blockListOne true))
               )) ;; end record one comes first
           ;; Manage the case where record two should be output.
           (else
            (begin
               (setq outBlock[(length outBlock)] recordTwo)
               (setq recordTwo (readBlockList blockListTwo true))
               )) ;; end record two comes first
            ) ;; end cond
        ;; Write the output block (if it is full).
        ;; Note: The output key is taken off the front
        ;;       of the list of output block keys. This
        ;;       means that the merged blocks are written
        ;;       back to the database using original keys.
        (if (>= (length outBlock) blockSize)
            (begin
               (setq outKey blockListOut[0])
               (delete blockListOut 0) 
               (setq myOR[outKey] outBlock)
               (setq outBlock (^new Vector: object: 0))
               )) ;; end if
        ) ;; end while
    ;; Write the output block (if it is not empty).
    ;; Note: The output key is taken off the front
    ;;       of the list of output block keys. This
    ;;       means that the merged blocks are written
    ;;       back to the database using original keys.
    (if (> (length outBlock) 0)
        (begin
           (setq outKey blockListOut[0])
           (delete blockListOut 0) 
           (setq myOR[outKey] outBlock)
           (setq outBlock #void)
           )) ;; end if
    ;; Always return true.
    true) ;; end of mergeBlockList





















;;**EXPORTKEY**:tableDbms:queryBuilder
(defChildLambda tableDbms:queryBuilder(aSpec)
;; *******************************************************************
;;  Summary: Builds a table screening function from the text specification   
;;           passed as an argument. The text specification is designed
;;           to allow ease of specification, recognition of similar 
;;           table screening strategies, and generation of table
;;           database screening strategies by genetic algorithms.
;; Note:     queryBuilder refers to building table screening Lambdas,
;;           not to building graphic user interface screens.
;; Depends:	 excludCriteriaLambda
;;           rulesLib
;;           selectCriteriaLambda
;; Args:     aSpec      Screen specification text.
;; Return:   proc       a table screening Lambda.   
;; *******************************************************************
   pvars:((debugOn false)         ;; Display generated Lambda on switch
          filterFields            ;; Temporary Dictionary of valid field names
          fixupLambda              ;; Lambda for expression transformation
          syntaxLambda             ;; Lambda for expression syntax validation
          ;; Methods list 
          assertRules             ;; Method for defining fix up rules
          buildBody               ;; Method to build the SmartLisp expression body
          buildName               ;; Method for recognizing a Legacy field name
          cmdAll                  ;; Method for parsing the "all" command
          cmdOmit                 ;; Method for parsing the "omit" command
          cmdSort                 ;; Method for parsing the "bottom" and "top" commands
          errorStop               ;; Method for error recovery
          mergeRules              ;; Method for merging "all" criteria together
          pairToVector            ;; Method for converting a list into a vector
          replaceRules            ;; Method for recursive criteria replacement
          ) ;; end of persistent variables
   vars:(proc aCmd i j m n cmds scmd theSpec cmdVector)
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> cmdAll #void) (goto Continue:))
   ;; Initialize the inline child Lambdas.
   (defun assertRules()
       ;; We only need to run this once.
       (if (isLambda fixupLambda) (return true))
       ;; Define the expression fix up rules.
       (rulesLib)
       (setq fixupLambda (new rulesLib))
       ;; Make sure there are no duplication errors
       (if (= fixupLambda.rulesDic rulesLib.rulesDic) (error "dupRules"))
       ;; Operator name, function name, and number recognition rules
       (fixupLambda.assert $OP:(lambda(x) 
                                 vars:(s (d #{and and or or min min max max + + - - * * / / ** expt < < <= <= = = <> <> >= >= > >})) 
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $REL:(lambda(x) vars:((d #{< < <= <= = = <> <> >= >= > >})) (if (isMember x d) d[x])))
       (fixupLambda.assert $FN1:(lambda(x) 
                                 vars:(s (d #{not not sin sin cos cos tan tan log log exp exp sqrt sqrt})) 
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $FN2:(lambda(x) 
                                 vars:(s (d #{min min max max}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (fixupLambda.assert $NAM:(lambda(x) 
                                 vars:((d #(today))) 
                                 (if (not (isMember x d)) x))
                                 ) ;; end assert
       (fixupLambda.assert $NUM:(lambda(x) (if (isNumber x) x)))
       ;; Infix to Prefix notation production rules
       (fixupLambda.assert $PFN:(lambda(a fn x y b) 
                                   vars:(p)
                                   (setq p (list fn x y)) 
                                   (if (<> b #void) (setq p (append (list p) b))) 
                                   (if (<> a #void) (setq p (append a p))) 
                                   p))
       (fixupLambda.assert '($a* $X <$FN=$OP> $Y $b*) '(<$PFN> $a $FN $X $Y $b))
       ;; Constant folding production rules
       (fixupLambda.assert $DIV:(lambda(x) (error "queryBuildExp" (append "queryBuilder divide by zero on: " (string x true)))))
       (fixupLambda.assert $FOLD1:(lambda(op x) vars:(f) (setq f (getGlobalValue (symbol op))) (f x)))
       (fixupLambda.assert $FOLD2:(lambda(op x y) vars:(f) (setq f (getGlobalValue (symbol op))) (f x y)))
       (fixupLambda.assert '(<$Y=$OP> <$X=$NUM> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '(<$Y=$FN1> <$X=$NUM>) '(<$FOLD1> $Y $X))
       (fixupLambda.assert '(<$Y=$FN2> <$X=$NUM> <$Z=$NUM>) '(<$FOLD2> $Y $X $Z))
       (fixupLambda.assert '(** $X $Y) '(expt $X $Y))
       ;; Algebraic expression reduction rules
       (fixupLambda.assert '(/ $X $X) 1)
       (fixupLambda.assert '(+ $X 0) '$X)
       (fixupLambda.assert '(- $X 0) '$X)
       (fixupLambda.assert '(* $X 0) 0)
       (fixupLambda.assert '(/ 0 0) 0)
       (fixupLambda.assert '(/ $X 0) '(<$DIV> (/ $X 0)))
       (fixupLambda.assert '(expt $X 0) 1)
       (fixupLambda.assert '(+ 0 $X) '$X)
       (fixupLambda.assert '(* 0 $X) 0)
       (fixupLambda.assert '(/ 0 $X) 0)
       (fixupLambda.assert '(expt 0 $X) 0)
       (fixupLambda.assert '(* $X 1) '$X)
       (fixupLambda.assert '(/ $X 1) '$X)
       (fixupLambda.assert '(expt $X 1) '$X)
       (fixupLambda.assert '(* 1 $X) '$X)
       (fixupLambda.assert '(expt 1 $X) 1)
       ;; _ANY_ empty indices reduction rules
       (fixupLambda.assert '(<$O=$REL> (ref (ref (ref x $X)) $Y) $Z) 
                          '(_ANY_ ($O (ref x $Y) $Z) (ref x $X)))
       ;; One based indices reduction rules
       (fixupLambda.assert '(ref (ref x $X) <$Y=$NUM>) '(ref (ref x $X) (sub1 $Y)))
       ;; Excess parentheses reduction rules
       (fixupLambda.assert '(($X*)) '$X)
       (fixupLambda.assert '(<$X=$NAM>) '$X)
       ;; Define the expression syntax validation rules.
       (setq syntaxLambda (new rulesLib))
       (syntaxLambda.setFailure false)
       ;; Make sure there are no duplication errors
       (if (= syntaxLambda.rulesDic fixupLambda.rulesDic) (error "dupRules"))
       ;; Prefix notation recognition rules
       (syntaxLambda.assert '(ref $X $Z) false)
       (syntaxLambda.assert '(<$Y=$FN1> <$X=$NUM>) '(<$FOLD1> $Y $X))
       (syntaxLambda.assert '(<$Y=$OP> $X $Z) false)
       (syntaxLambda.assert '(<$Y=$FN1> $X) false)
       (syntaxLambda.assert '(<$Y=$FN2> $X $Z) false)
       (syntaxLambda.assert '(today) false)
       ;; _ANY_ macro substitution rules
       (syntaxLambda.assert '(_ANY_ $A $B) 
                           '(mapc (lambda(x) (let ((r r)) (if $A (setq r true)) r)) $B))
       ;; Unrecognized syntax error rules
       (syntaxLambda.assert $ERR:(lambda(x) (error "queryBuildExp" (append "queryBuilder syntax error on: " (string x true)))))
       (syntaxLambda.assert '$ALL$ '(<$ERR> $ALL$))
       ;; Operator and function name recognition lambda rules
       (syntaxLambda.assert $OP:(lambda(x) 
                                 vars:(s (d #{and and or or min min max max + + - - * * / / expt expt < < <= <= = = <> <> >= >= > >}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FN1:(lambda(x) 
                                 vars:(s (d #{isBoolean isBoolean isNumber isNumber isDate isDate isSymbol isSymbol isString isString isDictionary isDictionary not not sin sin sub1 sub1 cos cos tan tan log log exp exp sqrt sqrt}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FN2:(lambda(x)
                                 vars:(s (d #{min min max max mapc mapc}))
                                 (if (isString x) (setq s (symbol x)) (setq s x)) 
                                 (if (isMember s d) d[s]))
                                 ) ;; end assert
       (syntaxLambda.assert $FOLD1:(lambda(op x) vars:(f) (setq f (getGlobalValue (symbol op))) (f x)))
       (syntaxLambda.assert $NUM:(lambda(x) (if (isNumber x) x)))
       true) ;; end assertRules
   (defun buildBody(v p begIndex endIndex)
   ;; Define the build expression body function. This function builds SmartLisp
   ;;  expression bodies.
   ;;  such as (1 {A1} true etc).
       vars:(fieldName s i n)
       (setq s "")
       (loop for i from begIndex until endIndex do
           (setq s (append s " " (buildName v[i] p)))
           ) ;; end loop
       ;; Apply expression transformation rules.
       (if (= p "x")
           (fixupLambda.setVerbose false)
           (fixupLambda.setVerbose false))
       (setq s (lisp s arithmetic:))
       (setq fixupLambda.singlePass false)
       (setq s (fixupLambda.apply s))
       (setq s (syntaxLambda.apply s))
       (setq s (string s true))
       s) ;; end buildBody
   (defun buildName(s p)
   ;; Define the build name function. This function recognizes Legacy
   ;;  field names inclosed in vertical bar | symbols, or SmarLisp constants
   ;;  such as (1 {A1} true etc).
       vars:(fieldName)
       (cond ;; Recognize a Legacy field name inclosed in vertical bar | symbols.
             ;;  Return the construct: p.|Fieldname|
             ((and (isType Symbol: s) (= s[0] #\|))
              (begin
                 (setq fieldName s)
                 (if (= (member fieldName _validFields) false)
                     (error "buildName"))
                 (setq filterFields[fieldName] _validFields[fieldName])
                 (append p "." fieldName)))
             ;; Recognize a Legacy field name not inclosed in vertical bar | symbols.
             ;;  Return the construct: p.|Fieldname|
             ((and (isType Symbol: s) (<> (member s _validFields) false))
              (begin
                 (setq fieldName s)
                 (setq filterFields[fieldName] _validFields[fieldName])
                 (append p "." fieldName)))
             ;; Recognize the today operator symbols.
             ;;  Convert the symbol to a List contant.
             ((= s today:)
              (append "(" s ")"))
             ;; Recognize all other non-arithmetic operator symbols.
             ;;  Convert the symbol to a string contant.
             ((and (isType Symbol: s) (= (member s #(isBoolean isNumber isDate isSymbol isString isDictionary and or not + - * / < > >= <= = <> ref |(| |)| |[| |]|)) false))
              (append "{" s "}"))
             ;; Repackage string constants within braces.
             ;;  Convert the string to a string contant.
             ((and (isString s) (= (member s #(isBoolean isNumber isDate isSymbol isString isDictionary and or not + - * / < > >= <= = <> ref |(| |)| |[| |]|)) false))
              (append "{" s "}"))
             ;; Recognize all quoted symbols.
             ;;  Convert the quoted symbol to a string contant.
             ((isType QuotedSymbol: s)
              (append "{" s "}"))
             ;; Assume we have a SmartLisp constant.
             ;;  Return the constant unaltered.
             (else s)
             ) ;; end cond
        ) ;; end buildName
   (defun cmdAll(parms s)
   ;; Define the "all" command parser.
   ;;   for example: "all = |Timeliness| 1"
       vars:(parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (= parmCnt 1)
           (return "")) 
       (if (< parmCnt 4)
           (error "queryBuilder" s)) 
       (if (<> parms[0] "all")
           (error "queryBuilder" s)) 
       (if (and (= parmCnt 4)
                (isBoolean (member parms[2] #("=" "<>" "<=" ">=" "<" ">"))))
           (error "queryBuilder" s))
       ;; Build the resulting find command.
       (setq outS (append "(select (lambda(x) (onError (lambda(s) false)) "
                          (buildBody parms "x" 1 parmCnt) "))"))
       outS) ;; end cmdAll
   (defun cmdOmit(parms s)
   ;; Define the "all" command parser.
   ;;   for example: "omit |Timeliness| = 5"
       vars:(parmCnt outS)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       ;; Check command for valid parameters.
       (if (= parmCnt 1)
           (return "")) 
       (if (< parmCnt 4)
           (error "queryBuilder" s)) 
       (if (<> parms[0] "omit")
           (error "queryBuilder" s)) 
       (if (and (= parmCnt 4)
                (isBoolean (member parms[2] #("=" "<>" "<=" ">=" "<" ">"))))
           (error "queryBuilder" s))
       ;; Build the resulting find command.
       (setq outS (append "(select (lambda(x) (onError (lambda(s) false)) (not "
                          (buildBody parms "x" 1 parmCnt) ")))"))
       outS) ;; end cmdOmit
   (defun cmdSort(parms s)
   ;; Define the "top", "bottom", & "sort" command parsers.
   ;;   for example: "top |Price| 5"
       vars:(parmCnt lastParm outS n ratio sortOp)
       ;; Break the command up into its parameters.
       (setq parmCnt (length parms))
       (setq lastParm (subi parmCnt 1))
       ;; Check command for number of arguments.
       (cond 
           ;; Manage simple truncate: "top cutoff"
           ((and (= parmCnt 2) (isMember parms[0] #(top: bottom:)))
            (begin 
               (if (= parms[0] "top")
                   (error "topInvalid")
                   (setq sortOp "<="))
               (cond ((isInteger parms[1])
                      (setq ratio (append "" parms[1])))
                     ((and (isNumber parms[1]) (< parms[1] 1))
                      (setq ratio (append "(integer (* LDB.recordCount " parms[1] "))")))
                     ((= parms[1] "all")
                      (setq ratio "LDB.recordCount"))
                     (else
                      (error "queryBuilder" parms))
                   ) ;; end cond
               ;; Build the resulting find command.
               (setq outS (append "(truncate " ratio ")"))))
           ;; Manage simple field sort: "top,value1,cutoff"
           ((and (= parmCnt 3) (isMember parms[0] #(top: bottom:)))
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">=")
                   (setq sortOp "<="))
               (cond ((isInteger parms[2])
                      (setq ratio (append "" parms[2])))
                     ((and (isNumber parms[2]) (< parms[2] 1))
                      (setq ratio (append "(integer (* LDB.recordCount " parms[2] "))")))
                     ((= parms[2] "all")
                      (setq ratio "LDB.recordCount"))
                     (else
                      (error "queryBuilder" parms))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(sort (lambda(x y) (onError (lambda(s) false)) (" sortOp
                                  " " (buildName parms[1] "x")
                                  " " (buildName parms[1] "y") ")))" _eol "   "))
               (setq outS (append outS "(truncate " ratio ")"))))
           ;; Manage simple expression sort: "top,value1,/,value2,cutoff" 
           ;; Note: (for backward compatibility).
           ((and (= parmCnt 5) 
                 (isMember parms[0] #(top: bottom:)) 
                 (isNumber (member parms[2] #("+" "-" "*" "/"))))
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">=")
                   (setq sortOp "<="))
               (cond ((isInteger parms[4])
                      (setq ratio (append "" parms[4])))
                     ((and (isNumber parms[4]) (< parms[4] 1))
                      (setq ratio (append "(integer (* LDB.recordCount " parms[4] "))")))
                     ((= parms[4] "all")
                      (setq ratio "LDB.recordCount"))
                     (else
                      (error "queryBuilder" parms))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(sort (lambda(x y) (onError (lambda(s) false)) (" sortOp
                                   " (" parms[2] " " (buildName parms[1] "x") " " (buildName parms[3] "x") ")"
                                   " (" parms[2] " " (buildName parms[1] "y") " " (buildName parms[3] "y") ")"
                                   ")))" _eol "   "))
               (setq outS (append outS "(truncate " ratio ")"))))
           ;; Manage complex expression sort: "top,/,value1,value1,cutoff" 
           (else
            (begin 
               (if (= parms[0] "top")
                   (setq sortOp ">=")
                   (setq sortOp "<="))
               (cond ((isInteger parms[lastParm])
                      (setq ratio (append "" parms[lastParm])))
                     ((and (isNumber parms[lastParm]) (< parms[lastParm] 1))
                      (setq ratio (append "(integer (* LDB.recordCount " parms[lastParm] "))")))
                     ((= parms[lastParm] "all")
                      (setq ratio "LDB.recordCount"))
                     (else
                      (error "queryBuilder" parms))
                   ) ;; end cond
               ;; Build the resulting sort find commands.
               (setq outS (append "(sort (lambda(x y) (onError (lambda(s) false)) (" sortOp " "
                                   (buildBody parms "x" 1 lastParm) " "
                                   (buildBody parms "y" 1 lastParm)
                                  ")))" _eol "   "))
               (setq outS (append outS "(truncate " ratio ")"))))
           ) ;end cond
       outS) ;; end cmdSort
   (defun errorStop(err) (writeln "queryBuilder: " err) false)
   (defun mergeRules(cmds)      
   ;; Merges consecutive "all" rules together with an "and" conjunction.
   ;; Looks for "all" rules and eliminates them.
       vars:(cmdIndex args newCmds ruleLambda i)
       (loop for cmdIndex from 0 until (length cmds) do
           (cond
              ;; Replace "all" command with nothing,
              ;; because and "all" command is a noop.
              ((= cmds[cmdIndex] "all")
               (begin
                  (setq cmds (delete cmds cmdIndex))
                  (setq cmdIndex (max (- cmdIndex 2) -1))
                  )) ;; end begin
              ;; Replace "remarks" command with nothing,
              ;; because and "remarks" command is a comment.
              ((= (left cmds[cmdIndex] 7) "remarks")
               (begin
                  (setq cmds (delete cmds cmdIndex))
                  (setq cmdIndex (max (- cmdIndex 2) -1))
                  )) ;; end begin
              ;; Replace "omit" command with "all (not )",
              ((= (left cmds[cmdIndex] 5) "omit ")
               (begin
                  (setq newCmds (append "all (not ("
                                        (mid cmds[cmdIndex] 5 100000)
                                        "))"))
                  (setq cmds[cmdIndex] newCmds)
                  (setq cmdIndex (max (- cmdIndex 2) -1))
                  )) ;; end begin
              ;; Merge consecutive "all" commands together,
              ;; with a conjunctive "and" operator.
              ((and (= (left cmds[cmdIndex] 4) "all ")
                    (< (add1 cmdIndex) (length cmds))
                    (= (left cmds[(add1 cmdIndex)] 4) "all "))
               (begin
                  (setq newCmds (append "all ("
                                        (mid cmds[cmdIndex] 4 100000)
                                        ") and ("
                                        (mid cmds[(add1 cmdIndex)] 4 100000)
                                        ")"))
                  (setq cmds (delete cmds cmdIndex))
                  (setq cmds[cmdIndex] newCmds)
                  (setq cmdIndex (max (sub1 cmdIndex) -1))
                  )) ;; end begin
               ) ;; end cond
           ) ;; end of loop
       cmds) ;; end of mergeRules
   (defun pairToVector(v p)
       vars:(i n result)
       (if (= v #void) (setq v (new Vector: 0)))
       (setq n (length p))
       (loop for i from 0 until n do
          (if (isAtom p[i])
              (setq v[(length v)] p[i])
              (begin
                 (setq v[(length v)] |(|:)
                 (setq v (pairToVector v p[i]))
                 (setq v[(length v)] |)|:)
                 )) ;; end if
          ) ;; end loop
       v) ;; end pairToVector
   (defun replaceRules(cmds)
   ;; Recursively replaces rules with their command strings.
   ;; Looks for "selection" rules in the selectCriteriaLambda.
   ;; Looks for "exclusion" rules in the excludCriteriaLambda.
       vars:(cmdIndex args newCmds ruleLambda i)
       (loop for cmdIndex from 0 until (length cmds) do
           ;; Check for rule command
           (if (= (left cmds[cmdIndex] 8) "criteria")
               (begin
                  (setq args (objectToVector (lisp cmds[cmdIndex] arithmetic:)))
                  ;; Assign the type of rule (selection or exclusion).
                  (cond ((= (length args) 2)
                         (setq ruleLambda selectCriteriaLambda))
                        ((and (= (length args) 3) (= args[2] selection:))
                         (setq ruleLambda selectCriteriaLambda))
                        ((and (= (length args) 3) (= args[2] exclusion:))
                         (setq ruleLambda excludCriteriaLambda))
                        (else 
                         (error "ruleType"))
                        ) ;; end cond
                  ;; Retrieve the rule string and 
                  ;; covert to a vector of commands
                  (if (and (= (left args[1] 1) "{") (= (right args[1] 1) "}")) 
                      (setq args[1] (mid args[1] 1 (subi (length args[1]) 2))))
                  (setq newCmds (ruleLambda.Pv.getCriteria args[1]))
                  (setq newCmds (stringToVector newCmds ";"))
                  ;; Replace substitute rules within the original 
                  ;; vector of command strings.
                  (setq cmds[cmdIndex] newCmds[0])
                  (loop for i from 1 until (length newCmds) do
                      (setq cmds (vectorInsert cmds (addi cmdIndex i) newCmds[i]))
                      ) ;; end loop
                  ;; Make sure we retest the first substituted rule.
                  (-- cmdIndex)
                  )) ;; end if
           ) ;; end of loop
       cmds) ;; end of replaceRules
   ;; Initialize the Lambda.
   Continue::
   (onError errorStop)
   (setq filterFields (new Dictionary:))
   (assertRules)
   ;; Break the command up into its sub-commands.
   (setq cmds (stringToVector aSpec ";"))
   ;; Perform recursive rule substitution.
   (setq cmds (replaceRules cmds)) 
   ;; Establish the filter names array.
   (loop for i from 0 until (length cmds) do
       ;; Establish the filter names for each command.
       (setq cmdVector (pairToVector #void (lisp cmds[i] arithmetic:)))
       (loop for j from 0 until (length cmdVector) do
           (buildName cmdVector[j] "x")
           ) ;; end j loop
       ) ;end i loop
   ;; Add the filter names array as additional restrictions (if "nocheck" command not present).
   (if (and (> (length cmds) 0) (<> (trim cmds[0]) "nocheck"))
       (begin
          (loop for i from 0 until (length filterFields) do
             (if (and (<> filterFields[i 1] #void) (<> filterFields[i 1] isDictionary:))
                 (begin
                    (setq aCmd (append "all (" filterFields[i 1] " |" filterFields[i 0] "|)"))
                    (setq cmds (vectorInsert cmds 0 aCmd))  
                    ))) ;; end loop
          )) ; end if
   ;; Perform rule merging.
   (setq cmds (mergeRules cmds))
   ;; Parse each sub command and append to the specification.
   (setq theSpec "")
   (loop for i from 0 until (length cmds) do
       ;; Call the proper command parser (based on the command word).
       (setq cmdVector (pairToVector #void (lisp cmds[i] arithmetic:)))
       (cond 
           ((= cmdVector[0] "all") (setq scmd (cmdAll cmdVector cmds[i]))) 
           ((= cmdVector[0] "check") (setq scmd ""))
           ((= cmdVector[0] "nocheck") (setq scmd ""))
           ((= cmdVector[0] "bottom") (setq scmd (cmdSort cmdVector cmds[i])))
           ((= cmdVector[0] "omit") (setq scmd (cmdOmit cmdVector cmds[i]))) 
           ((= cmdVector[0] "top") (setq scmd (cmdSort cmdVector cmds[i])))
           (else (error "queryBuilder" cmds[i]))
           ) ; end cond
       (setq theSpec (append theSpec scmd _eol "   "))
       ) ;end loop
   ;; Convert the screen specification into a procedure.
   (setq proc (append "(lambda(LDB)" _eol "   "))
   (setq proc (append proc "vars:(restore select sort truncate)" _eol "   "))
   (setq proc (append proc "(setq truncate LDB.truncate)" _eol "   "))
   (setq proc (append proc "(setq restore LDB.restore)" _eol "   "))
   (setq proc (append proc "(setq select LDB.select)" _eol "   "))
   (setq proc (append proc "(setq sort LDB.sort)" _eol "   "))
   (if (<> theSpec "")
       (setq proc (append proc theSpec)))
   (setq proc (append proc "true)"))
   (if (= debugOn true) (writeln proc)) ;; Use for testing only
   (setq proc (eval proc))
   (if (not (isLambda proc)) (error "queryBuilder"))
   proc) ;; end queryBuilder

;;  ***NOTES***:
;; 
;; see the screenBuilder notes, this Lambda implements the same query language.





















;;**EXPORTKEY**:tableDbms:readBlockList
(defChildLambda tableDbms:readBlockList(blockList eraseSW)
;; *******************************************************************
;; summary:  Read the next record from the specified list of blocks. 
;;           The blockList is a vector of block keys. As a new block
;;           is read, it is stored in the cdr of the blockList. As
;;           a block is exhausted, it is erased from the repository,
;;           and the blockList is altered to reflect the erased block.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     blockList   A vector of block keys to be read.
;;           eraseSW     True if blocks are to be erased after reading.
;; Return:   theRecord   The next database record (or #void).       
;; *******************************************************************
    vars:(blockKey theBlock theRecord) 
    ;; Return #void if there are no more records.
    (if (and (= (cdr blockList) #void) (<= (length blockList) 0)) (return #void))
    ;; Make sure the current block is loaded from the repository.
    ;; Note: If we must load a new block, then we
    ;;       erase the block from the repository
    ;;       and from the blockList after we load.
    (if (= (cdr blockList) #void)
        (begin
           (setq blockKey blockList[0])
           (setCdr blockList myOR[blockKey])
           (if (= eraseSW true)
               (setq myOR[blockKey] #void))
           (delete blockList 0)
           )) ;; end if
    ;; Read the next record from the current block.
    ;; Note: If we exhaust the current block, then we
    ;;       set the cdr of the blockList to #void.
    (setq theBlock (cdr blockList))
    (if (or (= theBlock #void) (<= (length theBlock) 0)) (return #void))
    (setq theRecord theBlock[0])
    (delete theBlock 0)
    (if (<= (length theBlock) 0) (setCdr blockList #void))
    ;; Return the next record.
    theRecord) ;; end of readBlockList




















;;**EXPORTKEY**:tableDbms:refColValues
(defChildLambda tableDbms:refColValues(col)
;; *******************************************************************
;; summary:  Return the column values vector for the specified column.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     col      Either the integer index of the column
;;                    of the name of the column as a string or
;;                    symbol.   
;; Return:   true
;; *******************************************************************
    vars:(colIndex currentColumn colKey)
    ;; Set table database to transaction mode
    (beginTransaction myOR)
    ;; Create the column key.
    (if (isNumber col)
        (begin
           (if (= colVector #void) (setq colVector myIndex._colVector))
           (setq colIndex (integer col))
           (setq colKey (makeSymbol (append "_column:" colVector[colIndex])))
           ) ;; end then
        (setq colKey (makeSymbol (append "_column:" col)))
        ); end if
    ;; Return the specified column values vector.
    (setq currentColumn myOR[colKey])
    ;; Set table database to privileged access
    (abortTransaction myOR)
    currentColumn) ;; end refColValues





















;;**EXPORTKEY**:tableDbms:renameBlocks
(defChildLambda tableDbms:renameBlocks(blockList anIndex)
;; *******************************************************************
;; summary:  Rename the specified list of blocks. The block keys are
;;           positive integers. Each key is renamed to its negation
;;           and restored in the specified repository index. The
;;           blockList is altered to reflect the new keys.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     blockList   A vector of block keys to be renamed.
;;           keyIndex    A Directory of block keys.
;; Return:   true        
;; *******************************************************************
    vars:(blockCount blockIndex oldKey newKey oldValue) 
    ;; Rename each block to its negation.
    ;; Note: This does not reposition the blocks  
    ;;       in the table database. The blocks
    ;;       are only renamed in the index.
    (setq blockCount (length blockList))
    (loop for blockIndex from 0 until blockCount do
        (setq oldKey blockList[blockIndex])
        (setq newKey (- oldKey))
        (setq blockList[blockIndex] newKey)
        (setq oldValue anIndex[oldKey])
        (setq anIndex[oldKey] #void)
        (setq anIndex[newKey] oldValue)
        ) ;; end of loop
    true) ;; end of renameBlocks




















;;**EXPORTKEY**:tableDbms:runStatistics
(defChildLambda tableDbms:runStatistics(statsLambda)
;; *******************************************************************
;; summary:  Run statistics for the current TableDbms database records.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     statsLambda    Statistics Lambda to gather record statistics.
;; Return:   recordCount   The number of records selected.     
;; *******************************************************************
    vars:(blockCount           ;; Count of record blocks in table database
          blockList            ;; Block list for 
          record               ;; Record to test against the selectLambda
          ) ;; end temporary variables
    ;; Select only the records which match the select Lambda.
    ;; Note: The old blocks are renamed and deleted
    ;;       as they are read. Only the matched 
    ;;       records are kept.
    (beginTransaction myOR)
    (setq blockCount myIndex._blockCount)
    (setq blockList (_makeBlockList 0 blockCount blockCount))
    (reset)
    (clear statsLambda)
    ;; Read each of the old records and keep the ones which
    ;; match the selectLambda. The kept records are written
    ;; back to the database, and the others are disgarded.
    (setq record (_readBlockList blockList true))
    (while (<> record #void)
        (statsLambda record)
        (setq record (_readBlockList blockList true))
        ) ;; end while
    (reset)
    (abortTransaction myOR)
    recordCount) ;; end of runStatistics





















;;**EXPORTKEY**:tableDbms:select
(defChildLambda tableDbms:select(selectLambda)
;; *******************************************************************
;; summary:  Select only those records which match the selectLambda.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     selectLambda   Select lambda value for matching records.
;; Return:   recordCount   The number of records selected.     
;; *******************************************************************
    vars:(blockCount           ;; Count of record blocks in table database
          blockList            ;; Block list for 
          record               ;; Record to test against the selectLambda
          ) ;; end temporary variables
    ;; Select only the records which match the select Lambda.
    ;; Note: The old blocks are renamed and deleted
    ;;       as they are read. Only the matched 
    ;;       records are kept.
    (beginTransaction myOR)
    (setq blockCount myIndex._blockCount)
    (setq blockList (_makeBlockList 0 blockCount blockCount))
    (_renameBlocks blockList myIndex)
    (restart)
    ;; Read each of the old records and keep the ones which
    ;; match the selectLambda. The kept records are written
    ;; back to the database, and the others are disgarded.
    (setq record (_readBlockList blockList true))
    (while (<> record #void)
        (if (selectLambda record) (addRow record))
        (setq record (_readBlockList blockList true))
        ) ;; end while
    (flush)
    (commitTransaction myOR)
    recordCount) ;; end of select





















;;**EXPORTKEY**:tableDbms:set1
(defChildLambda tableDbms:set1(row recordVector)
;; *******************************************************************
;; summary:  Add a row to the table database during Import processing. 
;;           Note: The table database is nonvolatile after import.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Args:     row           Row index (0 to n) of row to be added.
;;           recordVector  Row to be added to the table database.
;; Return:   true
;; Note:     This function is called from the importTab function whose
;;           setup processing is done in the importTableDbmsData Lambda.
;;           (see the tableDbms:importTableDbmsData Lambda).         
;; *******************************************************************
    ;; Only valid during the importTab function.
    (if (<> importingSW true) (error "NoAccess")) 
    ;; Allow numeric record keys only!
    (if (not (isNumber row)) (error "invalidKey")) 
    ;; Handle .SBF column header records.
    (if (and sbfFormatSW (= row 0))
        (begin 
            (setq myIndex._colVector recordVector) 
            (setq colVector recordVector)
            (setq myIndex._recordStructure (objectToStructure colVector #(#void)))
            (setq recordStructure myIndex._recordStructure)
            (if (compareNE (cnvColHeaders) colVector) (setq importFixupSW false))
            (return true)
            )) ;; end if
    ;; Handle separate column header records.
    (if (and (not sbfFormatSW) (= row 0))
        (begin
            (if (compareNE (cnvColHeaders) colVector) (setq importFixupSW false))
            (setq myIndex._colVector colVector)
            (setq myIndex._recordStructure (objectToStructure colVector #(#void)))
            (setq recordStructure myIndex._recordStructure)
            )) ;; end if
    ;; Ignore all other .SBF header records.
    (if (and sbfFormatSW (< row 3)) (return true))
    ;; Ignore all data records where the first field is #void.
    (if (= recordVector[0] #void)
        (begin
           (++ importNilRows)
           (return false)
           )) ;; end if
    ;; Reset record number to omit .SBF header records.
    (if sbfFormatSW (setq row (subi row 3)))
    (setq row (subi row importNilRows))
    ;; We get here only for valid import data records.
    ;; Fix up the newly imported data record.
    (resize recordVector (length myIndex._colVector))
    ;(if (and (= importFixupSW true) (isType Lambda: importFixup))
    (if (isType Lambda: importFixup)
        (importFixup recordVector myIndex._tableDate))
    ;; Compute block and record numbers.
    (setq blockKEY (divi row blockSize))
    (setq recordNUM (modi row blockSize))
    ;; Save the record in the current block
    (if (= currentBlock #void)
        (setq currentBlock (new Vector: object: 1)))
    (setq currentBlock[recordNUM] (objectToDictionary recordVector))
    ;; Save the block (if necessary)
    (if (>= (length currentBlock) blockSize)
        (begin
            (writeln "Importing record #" row)
            (setq myOR[blockKEY] currentBlock)
            (setq myIndex._blockCount (addi blockKEY 1))           
            (setq currentBlock #void)))
    (setq myIndex._recordCount (addi row 1))
    (setq recordCount myIndex._recordCount)
    true) ;; end of set1





















;;**EXPORTKEY**:tableDbms:smarttableToTableDbms
(defChildLambda tableDbms:smarttableToTableDbms(ST)
;; ********************************************************************
;; summary:  Save the specified Smarttable into the table database.
;;           after running production fixup.
;; Parms:    ST      The Smarttable of fixed up table data.
;; Return    true
;; ********************************************************************
    vars:(i j m colCount recCount record rowIndex colIndex)
    ;; Set table database to transaction mode
    (beginTransaction myOR)
    ;; Compute the column count and record count.
    (if (= colVector #void) (setq colVector myIndex._colVector))
    (setq colCount (length colVector))
    (setq recCount recordCount)
    ;; Save each row of the Smarttable into
    ;; the appropriate table database record.
    (loop for rowIndex from 0 until recCount do
        (setq record (copy recordStructure))
        (loop for colIndex from 0 until colCount do
            (setq record[colIndex] ST[colIndex rowIndex])
            ) ;; end of col loop
        (setRowSeq rowIndex record)
        ) ;; end of row loop
    ;; Set table database out of transaction mode.
    (commitTransaction myOR)
    true) ;; end tableToSmarttable





















;;**EXPORTKEY**:tableDbms:sort
(defChildLambda tableDbms:sort(sortLambda)
;; *******************************************************************
;; summary:  Sort the entire table database.
;; parent:   Librarian Lambda which manages the table database and
;;           controls the table database schema.
;; Notes:    This algorithm will increase the sort cluster factor
;;           during the critical initial sort phase (to match the
;;           available RAM memory). 
;; Args:     sortLambda   Sort lambda value for record pairs.
;; Return:   true        
;; *******************************************************************
    pvars:(;; Methods List
           _sortReadBigBlock   ;; Read many smaller blocks into the currentBlock, which becomes a giant block for sorting.
           _sortWriteBigBlock  ;; Break the currentBlock up into blocks of the correct blocking factor, then write each smaller block to the database.
           ) ;; end of persistent variables
    vars:(blockCount           ;; Count of record blocks in table database
          blockListOne         ;; Block list one for merge
          blockListTwo         ;; Block list two for merge
          clusterIndex         ;; Index for scanning clustered blocks
          clusterSize          ;; Size of clustered blocks (always a log of 2)
          endTime              ;; Ending time for sort
          freeSpace            ;; Number of bytes of free space
          mergeIndex           ;; Index of the current block to merge
          mergePassIndex       ;; Index of the current merge pass
          mergePassCount       ;; Number of merge passes required for this sort
          mergePassStart       ;; Starting value of the merge pass index
          mergeStepSize        ;; Number of blocks in each merge pass block list pair
          mergeRunCount        ;; Number of blocks in each merge pass block list
          recordOne            ;; Record one for merge
          recordTwo            ;; Record two for merge
          startTime            ;; Starting time for sort
          ) ;; end temporary variables
    ;; Never initialize this Lambda more than once, because the
    ;; inline child Lambdas will overlay the cloned child Lambdas
    ;; in any clone copies of the Lambda and this causes serious
    ;; confusion when the reinitialized clone begins to affect
    ;; the persistant storage of the original Lambda.
    (if (<> _sortReadBigBlock #void) (goto Continue:))
    (defun _sortReadBigBlock(blockIndex clusterSize blockCount)
        ;; Read many smaller blocks into the currentBlock,
        ;; which becomes a giant block for sorting.
        ;; Note: We have already read the first block 
        ;;       into currentBlock.
        vars:(newBlock clusterIndex stopIndex)
        (setq stopIndex (min clusterSize (subi blockCount blockIndex)))
        (beginTransaction myOR)
        (loop for clusterIndex from 1 until stopIndex do
            (setq newBlock myOR[(addi blockIndex clusterIndex)])
            ;(gc)(writeln "Read block [" (addi blockIndex clusterIndex) "] size = [" (sizeof newBlock) "] free = [" (inspect))  
            (setq currentBlock (append currentBlock newBlock))
            ) ;; end loop
        (abortTransaction myOR)
        true) ;; end of _sortReadBigBlock
    (defun _sortWriteBigBlock(blockIndex)
        ;; Break the currentBlock up into blocks of the correct blocking
        ;; factor, then write each smaller block to the database.
        vars:(newBlock clusterIndex clusterSize recordIndex blockStart blockLen)
        (commitTransaction myOR)
        (setq clusterSize (ceiling (/ (length currentBlock) blockSize)))
        (if (= clusterSize 1) (return (setq myOR[blockIndex] currentBlock)))
        (loop for clusterIndex from 0 until clusterSize do
            (setq blockStart (muli clusterIndex blockSize))
            (setq newBlock (^new Vector: object: (min blockSize (subi (length currentBlock) blockStart))))
            (setq blockLen (length newBlock))
            (loop for recordIndex from 0 until blockLen do
                (setq newBlock[recordIndex] currentBlock[(addi recordIndex blockStart)])
                ) ;; end recordIndex loop
            (setq myOR[(addi blockIndex clusterIndex)] newBlock)
            ) ;; end clusterIndex loop
        true) ;; end of _sortWriteBigBlock
    ;; Initialize the Lambda when called by new ObjectRepository: or attachLibrarian. 
    ;; This function always reattaches itself to the Repository (if necessary),
    ;; and always reestablishes the table block count, record count, and other statistics.
    Continue::
    ;; Sort the records in each block.
    ;; Note: This is the first part of a merge  
    ;;       sort of the entire table database.
    (beginTransaction myOR)
    (setq startTime (getTickCount 0))
    (setq blockCount myIndex._blockCount)
    (gc)
    (setq freeSpace (- (inspect) 10000000))
    (setq clusterSize 1)
    (commitTransaction myOR)
    (loop for blockKEY from 0 until blockCount do
        (setq currentBlock myOR[blockKEY])
        ;; Compute the cluster size (if this is the first block)
        (if (and (= blockKEY 0) (isPositive freeSpace))
            (begin
               (setq clusterSize (divi freeSpace (sizeof currentBlock) 4))
               (setq clusterSize (expt 2 (integer (log2 clusterSize))))
               (setq clusterSize (min clusterSize blockCount 16))
               ;(writeln "Cluster size = [" clusterSize "]")
               )) ;; end if
        ;; Read and sort blocks in clusters according to cluster size
        (_sortReadBigBlock blockKEY clusterSize blockCount)
        (^sort currentBlock sortLambda)
        (_sortWriteBigBlock blockKEY)
        ;; Adjust block index for size of cluster
        (setq blockKEY (sub1 (addi blockKEY clusterSize)))
        ) ;; end of loop
    (commitTransaction myOR)
    (setq currentBlock #void)
    (setq blockKEY #void)
    ;(writeln "Completed first pass sort.")
    ;; Perform the merge phase of the sort.
    ;; Note:  The number of merge passes is always equal
    ;;        to the log 2 of the number of blocks.
    ;; For Example:
    ;;        The merge phase of the sort for a database
    ;;        of seven blocks would appear as follows.
    ;;        (_mergeBlockList (makeVector 1 0) (makeVector 1 1) sortLambda)
    ;;        (_mergeBlockList (makeVector 1 2) (makeVector 1 3) sortLambda)
    ;;        (_mergeBlockList (makeVector 1 4) (makeVector 1 5) sortLambda)
    ;;        (_mergeBlockList (makeVector 1 6) (makeVector 0 0) sortLambda)
    ;;        (_mergeBlockList (makeVector 2 0 1) (makeVector 2 2 3) sortLambda)
    ;;        (_mergeBlockList (makeVector 2 4 5) (makeVector 1 6) sortLambda)
    ;;        (_mergeBlockList (makeVector 4 0 1 2 3) (makeVector 3 4 5 6) sortLambda)
    (if (< clusterSize blockCount)
        (begin
           (setq mergePassCount (integer (log2 blockCount)))
           (setq mergePassStart (integer (log2 clusterSize)))
           (if (<> mergePassCount (log2 blockCount)) (++ mergePassCount))
           (loop for mergePassIndex from mergePassStart until mergePassCount do
               ;(writeln "Starting merge pass [" mergePassIndex "] of [" mergePassCount "]")
               (setq mergeStepSize (expt 2 (add1 mergePassIndex))) 
               (setq mergeRunCount (divi mergeStepSize 2)) 
               (setq mergeIndex 0)
               (while (< mergeIndex blockCount)
                   (setq blockListOne (_makeBlockList mergeIndex mergeRunCount blockCount))
                   (setq blockListTwo (_makeBlockList (+ mergeIndex mergeRunCount) mergeRunCount blockCount))
                   (_mergeBlockList blockListOne blockListTwo sortLambda)
                   (+= mergeIndex mergeStepSize) 
                   ) ;; end merge stepping while
               ) ;; end merge passes loop
           )) ;; end merge if
    (setq endTime (integer (getTickCount startTime)))
    ;(writeln "Records sorted = " recordCount " in " endTime " Seconds" )
    true) ;; end of sort





















;;**EXPORTKEY**:tableDbms:tableLambdaToTableDbms
(defChildLambda tableDbms:tableLambdaToTableDbms(ST)
;; ********************************************************************
;; summary:  Save the specified tableLambda into the table database.
;; Parms:    ST      The tableLambda of table data.
;; Return    true
;; ********************************************************************
    vars:(i j m colCount recCount record rowIndex colIndex)
    ;; Set table database to transaction mode
    (beginTransaction myOR)
    ;; Compute the column count and record count.
    (if (= colVector #void) (setq colVector myIndex._colVector))
    (setq colCount (length colVector))
    (setq recCount recordCount)
    ;; Save each row of the tableLambda into
    ;; the appropriate table database record.
    (loop for rowIndex from 0 until recCount do
        (setRowSeq rowIndex ST[rowIndex])
        ) ;; end of row loop
    ;; Set table database out of transaction mode.
    (commitTransaction myOR)
    true) ;; end tableLambdaToTableDbms





















;;**EXPORTKEY**:tableDbms:tableToHistoryLambda
(defChildLambda tableDbms:tableToHistoryLambda(tableSize startRow rowIncr)
;; ********************************************************************
;; summary:  Convert the current table database into a tableLambda
;;           for running production selection criteria and for 
;;           production fixup.
;; Parms:    tableSize  The size of the history tableLambda.    
;; Return    ST         The tableLambda of table data.
;; ********************************************************************
    vars:(ST cols i j m colCount recCount 
          tableIncrement tableRow tableDate
          record rowIndex colIndex
          ) ;; end temporary variables
    ;; Set table database to transaction mode
    (beginTransaction myOR)
    ;; Compute the startRow, endRow and rowIncrement values.
    (setq recCount recordCount)
    (setq tableIncrement rowIncr)
    (setq tableRow startRow)
    ;; Retrieve the table database date.
    (setq tableDate myIndex._tableDate)
    (if (= tableDate #void)
        (begin
           (setq tableDate (now))
           (setq myIndex._tableDate tableDate)
           )) ;; end if
    ;; Retrieve the column names and make the tableLambda.
    (if (= colVector #void) (setq colVector myIndex._colVector))
    (setq colCount (length colVector))
    (tableLambda)
    (setq ST (new tableLambda))
    (ST.setSparse true)
    (ST.defColumns colVector)
    ;; Read each table database record
    ;; and add a new row to the tableLambda.
    (reset)
    (updateBegin)
    (loop for rowIndex from 0 until tableSize do
        (if (= (modi rowIndex 100) 0) 
            (writeln "Archiving record #" rowIndex " [Mfree = " (inspect) "] {TSize = " (sizeof ST) "}"))
        (setq record (updateRead tableRow))
        (ST.addRow record)
        (setq tableRow (integer (+ tableRow tableIncrement)))
        (if (>= tableRow recordCount) (goto OutOfRecords:))
        ) ;; end of row loop
	OutOfRecords::
    (updateEnd)
    ;; Set table database out of transaction mode.
    (abortTransaction myOR)
    ST) ;; end tableToHistoryLambda





















;;**EXPORTKEY**:tableDbms:tableToSmarttable
(defChildLambda tableDbms:tableToSmarttable(...)
;; ********************************************************************
;; summary:  Convert the current table database into a Smarttable
;;           for running production selection criteria and for 
;;           production fixup.
;; Parms:    startRow   The starting row (optional).    
;;           endingRow  The ending row (optional).    
;; Return    ST         The Smarttable of table data.
;; ********************************************************************
    vars:(ST cols i j m colCount recCount 
          record rowIndex colIndex 
          tableDate startRow endRow
          ) ;; end temporary variables
    ;; Set table database to transaction mode
    (beginTransaction myOR)
    ;; Retrieve the optional startRow and endRow arguments.
    (setq recCount recordCount)
    (if (= (argCount) 2)
        (begin
           (setq startRow (argFetch 0))
           (setq endRow (argFetch 1))
           ) ;; end then
        (begin
           (setq startRow 0)
           (setq endRow recCount)
           ) ;; end then
        ) ;; end if
    (if (< startRow 0) (setq startRow 0))
    (if (> endRow recCount) (setq endRow recCount))
    (if (>= startRow endRow)
        (begin
           (setq startRow 0)
           (setq endRow recCount)
           )) ;; end then
    ;; Retrieve the table database date.
    (setq tableDate myIndex._tableDate)
    (if (= tableDate #void)
        (begin
           (setq tableDate (now))
           (setq myIndex._tableDate tableDate)
           )) ;; end if
    ;; Create the column names and type structure.
    (if (= colVector #void) (setq colVector myIndex._colVector))
    (setq colCount (length colVector))
    (setq cols (new Vector: (muli colCount 2)))  
    (loop for i from 0 until colCount do
        (setq j (muli i 2))
        (setq cols[j] (symbol colVector[i]))
        (setq cols[(addi j 1)] formula:)
        ) ;; end of loop
    (setq cols (objectToStructure cols))
    ;; Make the Smarttable shell
    (setq ST (new Smarttable: cols))
    ;; Read each table database record
    ;; and create a new row of the Smarttable.
    (reset)
    (loop for rowIndex from startRow until endRow do
        (setq record (refRowSeq rowIndex))
        (loop for colIndex from 0 until colCount do
            (setq ST[colIndex (subi rowIndex startRow)] record[colIndex])
            ) ;; end of col loop
        ) ;; end of row loop
    ;; Set table database out of transaction mode.
    (abortTransaction myOR)
    ST) ;; end tableToSmarttable





















;;**EXPORTKEY**:tableDbms:tableToTableLambda
(defChildLambda tableDbms:tableToTableLambda(...)
;; ********************************************************************
;; summary:  Convert the current table database into a tableLambda
;;           for running production selection criteria and for 
;;           production fixup.
;; Parms:    startRow   The starting row (optional).    
;;           endingRow  The ending row (optional).    
;; Return    ST         The tableLambda of table data.
;; ********************************************************************
    vars:(ST cols i j m colCount recCount 
          record rowIndex colIndex 
          tableDate startRow endRow
          ) ;; end temporary variables
    ;; Set table database to transaction mode
    (beginTransaction myOR)
    ;; Retrieve the optional startRow and endRow arguments.
    (setq recCount recordCount)
    (if (= (argCount) 2)
        (begin
           (setq startRow (argFetch 0))
           (setq endRow (argFetch 1))
           ) ;; end then
        (begin
           (setq startRow 0)
           (setq endRow recCount)
           ) ;; end then
        ) ;; end if
    (if (< startRow 0) (setq startRow 0))
    (if (> endRow recCount) (setq endRow recCount))
    (if (>= startRow endRow)
        (begin
           (setq startRow 0)
           (setq endRow recCount)
           )) ;; end then
    ;; Retrieve the table database date.
    (setq tableDate myIndex._tableDate)
    (if (= tableDate #void)
        (begin
           (setq tableDate (now))
           (setq myIndex._tableDate tableDate)
           )) ;; end if
    ;; Retrieve the column names and make the tableLambda.
    (if (= colVector #void) (setq colVector myIndex._colVector))
    (setq colCount (length colVector))
    (tableLambda)
    (setq ST (new tableLambda))
    (ST.setSparse true)
    (ST.defColumns colVector)
    ;; Read each table database record
    ;; and add a new row to the tableLambda.
    (reset)
    (loop for rowIndex from startRow until endRow do
        (setq record (refRowSeq rowIndex))
        (ST.addRow record)
        ) ;; end of row loop
    ;; Set table database out of transaction mode.
    (abortTransaction myOR)
    ST) ;; end tableToTableLambda





















;;**EXPORTKEY**:tableDbms:updateBegin
(defChildLambda tableDbms:updateBegin()
;; *******************************************************************
;; Summary:  Prepares the table database Lambda for an extended series 
;;           of reads for update followed by writes of the rows
;;           previously read. 
;; Note:     Reads do not have to be sequential.
;;           Writes do not have to be sequential.
;;           Each update session must be terminated with an updateEnd
;; Args:     none
;; Return:   true
;; *******************************************************************
    (if updateSW (updateEnd))
    (setq updateSW true)
    (setq updateDirty false)
    (setq blockKEY #void)
    (setq currentBlock #void)
    (setq recordNUM 0)
    true) ;; end of updateBegin



















;;**EXPORTKEY**:tableDbms:updateEnd
(defChildLambda tableDbms:updateEnd()
;; *******************************************************************
;; Summary:  Terminates an extended series of reads for update followed
;;           by writes of the rows previously read. 
;; Note:     Reads do not have to be sequential.
;;           Writes do not have to be sequential.
;;           Each update session must be started with an updateBegin
;; Args:     none
;; Return:   true
;; *******************************************************************
    (if (not updateSW) (return false))
    ;; Save current block?
    (if updateDirty (setq myOR[blockKEY] currentBlock))
    (setq updateSW true)
    (setq updateDirty false)
    (setq blockKEY #void)
    (setq currentBlock #void)
    (setq recordNUM 0)
    true) ;; end of updateEnd



















;;**EXPORTKEY**:tableDbms:updateRead
(defChildLambda tableDbms:updateRead(row)
;; *******************************************************************
;; Summary:  Reads a record from the table database for later update.
;; Note:     Reads do not have to be sequential.
;;           Writes do not have to be sequential.
;;           Each update session must be started with an updateBegin
;; Args:     row      Row number of row to be read for later update.
;; Return:   record   The row just read. 
;; *******************************************************************
    vars:(newBlockKEY)
    (if (not updateSW) (error "accessDenied"))
    (if (or (< row 0) (>= row recordCount)) (error "badRowIndex"))
    ;; Compute new block and record numbers.
    (setq newBlockKEY (divi row blockSize))
    (setq recordNUM (modi row blockSize))
    ;; Need different block of records?
    (if (<> newBlockKEY blockKEY)
        (begin
           ;; Save current block?
           (if updateDirty (setq myOR[blockKEY] currentBlock))
           (setq updateDirty false)
           (setq blockKEY newBlockKEY)
           (setq currentBlock myOR[blockKEY])
           )) ;; end if
    ;; Return the requested record for later update.
    currentBlock[recordNUM]) ;; end of updateRead



















;;**EXPORTKEY**:tableDbms:updateReadStructure
(defChildLambda tableDbms:updateReadStructure(row)
;; *******************************************************************
;; Summary:  Reads a record (converted to a Structure) from the table
;;           database for later update.
;; Note:     Reads do not have to be sequential.
;;           Writes do not have to be sequential.
;;           Each update session must be started with an updateBegin
;; Args:     row      Row number of row to be read for later update.
;; Return:   record   The row just read (converted to a Structure). 
;; *******************************************************************
    vars:(record)
	(setq record (updateRead row))
    ;; Return the requested record (converted to a Structure) for later update.
    (setq record (objectToStructure (copy recordStructure) record))
    record) ;; end of updateReadStructure



















;;**EXPORTKEY**:tableDbms:updateWrite
(defChildLambda tableDbms:updateWrite(row record)
;; *******************************************************************
;; Summary:  Write a record from the table database during an extended
;;           update transaction.
;; Note:     Reads do not have to be sequential.
;;           Writes do not have to be sequential.
;;           Each update session must be started with an updateBegin
;; Args:     row      Row number of row to be read for later update.
;;           record   Row to be written to the table database.
;; Return:   record   The row just written. 
;; *******************************************************************
    vars:(newBlockKEY)
    (updateRead row)
    (setq updateDirty true)
    ;; Return the record just written.
    (setq currentBlock[recordNUM] record)
    record) ;; end of updateWrite



















;;**EXPORTKEY**:updateRowByKey
(defun updateRowByKey(SS key)
;; *************************************************************************************
;; summary:  This procedure gets a SmartRow from the specified Smarttable
;;           using the values in the key attributes of the key object. The key object
;;           may be any object which can be indexed by the key column names of the
;;           specified Smarttable. If there is no matching row, a new row is added with
;;           the specified key values.
;; args:     SS:                 The Smarttable to be searched for the row.
;;           key:                The key object whose attributes are used to find the row.
;; return:   row:                The SmartRow whose key attributes match the key object,
;;                               a new SmartRow whose key attributes are set to match 
;;                               the key object, or an error if there is more than one
;;                               SmartRow whose key attributes match the key object.
;; *************************************************************************************
   vars:(i r n colCount row col valueIndex value rowIndex colVector)
   ;; If there is a matching row, then return it.
   (setq row (getRowByKey SS key))
   (if (<> row #void) (return row))
   ;; Get the next available row index.
   (setq row (length SS))
   ;; Search the Smarttable columns looking for key columns.
   (setq colCount SS.colCount)
   (setq colVector SS.colVector)
   (loop for i from 0 until colCount do
      (if (= colVector[i].colFormat key:)
          (begin
             ;; Use the key column's name to get the index value from the key object.
             (setq value key[colVector[i].name])
             (setq col colVector[i].cellIndex)
             ;; Set the index value from the key object into the new row.
             (set SS col row value))))
   ;; Return the newly added row.
   SS[row])




























;;**EXPORTKEY**:validateColHeaders
(defun validateColHeaders(colHeaders)
;; ********************************************************************
;; summary:  Validate the legacy database column headers.
;; Parms:    colHeaders   The legacy data column headers.
;; Return    true
;; ********************************************************************
    vars:(i j m colCount colIndex)
    ;; There must be exactly the right number of column headers
    (setq colCount (length _validFields))
    (if (<> (length colHeaders) colCount) (error "badColHeaders"))
    ;; Each column header must match the valid column headers
    (loop for colIndex from 0 until colCount do
        (if (<> _validFields[colIndex 0] colHeaders[colIndex]) (error "badColHeaders"))
        ) ;; end of loop
    true) ;; end validateColHeaders





























;;**EXPORTKEY**:vectorAdd
(defun vectorAdd(x y)
;; *******************************************************************
;; summary:  Returns the sum of two vectors or of a vector and a constant. 
;;           If x and y are vectors of equal length,
;;           then x+y is: #(x[0]+y[0]  x[1]+y[1] .... x[N]+y[N]).
;;           If x is a vector and y is a constant,
;;           then x+y is: #(x[0]+y  x[1]+y .... x[N]+y).
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N or a constant.
;; Return:   v:       The vector sum of x+y
;; *******************************************************************
    vars:(i m n v)
    (setq n (length x))
    (setq v (new Vector: n))
    (if (isNumber y)
        (loop for i from 0 until n do
            (setq v[i] (+ x[i] y)))
        (begin
            (setq m (length y))
            (if (<> n m)
                (begin 
                   (ringBell)
                   (writeln "vectorAdd: vectors not the same length")
                   (writeln "(length x)=" n)
                   (writeln "(length y)=" m)
                   (writeln "x=" x)
                   (writeln "y=" y)
                   (setq n (min m n))))
            (loop for i from 0 until n do
                (setq v[i] (+ x[i] y[i])))))
    v)





























;;**EXPORTKEY**:vectorDotProduct
(defun vectorDotProduct(x y)
;; *******************************************************************
;; summary:  Returns the dot product of two vectors. If x and y are 
;;           vectors of equal length, 
;;           then xùy is: x[0]y[0] + x[1]y[1] + .... x[N]y[N].
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N.
;; Return:   v:       The product of xùy
;; *******************************************************************
    vars:(i m n sum)
    (setq n (length x))
    (setq m (length y))
    (if (<> n m)
        (begin 
            (ringBell)
            (writeln "vectorDotProduct: vectors not the same length")
            (writeln "(length x)=" n)
            (writeln "(length y)=" m)
            (writeln "x=" x)
            (writeln "y=" y)
            (setq n (min m n))))
    (loop for i from 0 until n do
        (+= sum (* x[i] y[i])))
    sum)





























;;**EXPORTKEY**:vectorProduct
(defun vectorProduct(x y)
;; *******************************************************************
;; summary:  Returns the product of two vectors or of a vector and a constant. 
;;           If x and y are vectors of equal length,
;;           then xøy is: #(x[0]y[0]  x[1]y[1] .... x[N]y[N]).
;;           If x is a vector and y is a constant,
;;           then xøy is: #(x[0]y  x[1]y .... x[N]y).
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N or a constant.
;; Return:   v:       The vector product of xøy
;; *******************************************************************
    vars:(i m n v)
    (setq n (length x))
    (setq v (new Vector: n))
    (if (isNumber y)
        (loop for i from 0 until n do
            (setq v[i] (* x[i] y)))
        (begin
            (setq m (length y))
            (if (<> n m)
                (begin 
                   (ringBell)
                   (writeln "vectorProduct: vectors not the same length")
                   (writeln "(length x)=" n)
                   (writeln "(length y)=" m)
                   (writeln "x=" x)
                   (writeln "y=" y)
                   (setq n (min m n))))
             (loop for i from 0 until n do
                 (setq v[i] (* x[i] y[i])))))
    v)





























;;**EXPORTKEY**:vectorSub
(defun vectorSub(x y)
;; *******************************************************************
;; summary:  Returns the difference of two vectors or of a vector and
;;           a constant. 
;;           If x and y are vectors of equal length,
;;           then x-y is: #(x[0]-y[0]  x[1]-y[1] .... x[N]-y[N]).
;;           If x is a vector and y is a constant,
;;           then x+y is: #(x[0]-y  x[1]-y .... x[N]-y).
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N or a constant.
;; Return:   v:       The vector difference of x+y
;; *******************************************************************
    vars:(i m n v)
    (setq n (length x))
    (setq v (new Vector: n))
    (if (isNumber y)
        (loop for i from 0 until n do
            (setq v[i] (- x[i] y)))
        (begin
            (setq m (length y))
            (if (<> n m)
                (begin 
                   (ringBell)
                   (writeln "vectorSub: vectors not the same length")
                   (writeln "(length x)=" n)
                   (writeln "(length y)=" m)
                   (writeln "x=" x)
                   (writeln "y=" y)
                   (setq n (min m n))))
            (loop for i from 0 until n do
                (setq v[i] (- x[i] y[i])))))
    v)






























;;**EXPORTKEY**:vectorToDelimitedString
(defun vectorToDelimitedString(vec delimiter)
;; ********************************************************************
;; name:     vectorToDelimitedString
;; 
;; summary:  Convert the specified vector into a Spreadsheet
;;                  Lambda for easy viewing.
;; Parms:      vec:          The vector to be converted.
;;                  delimiter:   The delimiter for the result string.
;; Return:     ss:             The resulting delimited string
;; ********************************************************************
    vars:(ss i m)
    (setq m (subi (length vec) 1))
    (setq ss "")
    (loop for i from 0 until m do
         (setq ss (append ss vec[i] delimiter))
         ) ;; end of loop
    (setq ss (append ss vec[m]))
    ss)





























;;**EXPORTKEY**:vectorToSpreadsheet
(defun vectorToSpreadsheet(v)
;; ********************************************************************
;; summary:  Convert the specified vector into a Spreadsheet
;;           Lambda for easy viewing.
;; Parms:    v:       The vector to be converted.
;;           ss:      The Spreadsheet version of the vector.
;; ********************************************************************
    vars:(ss i m)
    (setq m (length v))
    (setq ss (new Spreadsheet:))
    (loop for i from 0 until m do
            (setq ss[i 0] v[i]))
    ss)





























;;**EXPORTKEY**:wholeWordReplace
(defun wholeWordReplace(findme replaceme sourceVec)
;; *******************************************************************
;; summary:  This procedure replaces whole words with other whole words.
;; args:     findme        The target word to be replaced.   
;;           replaceme     The new replacement word.   
;;           sourceVec     The source file data to be altered.   
;; return:   sourceVec     The source file data after alteration.   
;; *******************************************************************
    vars:(sourceLen i j m n startPos findLEN replaceLEN)
    ;; Define alpha numeric check.
    (defun isAlphanum(cc) (or (isCharAlphabetic cc) (isCharNumeric cc)))
    ;; Find the length of the source data.
    (setq sourceLen (length sourceVec))
    (setq findLEN (length findme))
    (setq replaceLEN (length replaceme))
    ;; Replace target whole word with the replacement whole word.
    (loop for j from 0 until sourceLen do
       (if (isBoolean (setq startPos (find findme sourceVec j)))
           (setq j sourceLen)
           (begin
               (cond ((and (> startPos 0) (isAlphanum sourceVec[(sub1 startPos)])) (setq j (add1 startPos)))
                     ((and (< startPos (- sourceLen findLEN)) (isAlphanum sourceVec[(+ startPos findLEN)])) (setq j (add1 startPos)))
                     (else (setq sourceLen (length (setq sourceVec (replace sourceVec startPos findLEN replaceme)))))))))
    ;; Return the altered source data.
    sourceVec)































;;**EXPORTKEY**:writeTextFile
(defun writeTextFile(name self) 
;; *******************************************************************
;; summary:  Perform a complete write of the specified text file.
;; Parms:    This procedure accepts two arguments.
;;           name:      The name of the file to create.
;;           self:      The object to be saved in the file.
;; *******************************************************************
   vars:(fileID (type 0))
   (setq fileID (fileOpen name 1 type))
   (fileWrite fileID self)
   (fileClose fileID 1)
   self)







