;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************

;;**EXPORTKEY**:index

(defun index()
;;************************************************************************
;; Index Lambda
;; The index object provides a persistant index object similar to a directory
;; with the following advanced features: 
;; Multiple component keys (composite keys).
;; Search on partial keys (leading components).
;; Unique and Non-unique index management.
;;
;; Architecture:
;; The index is comprised of a nested hierarchy of directories. 
;; Each directory level corrosponds to a component of the composite index key.
;; Each entry in a directlry level is a node.
;; The nodes of all but the lowest level in the hierarchy are pairs that contain
;; a directory entry in the pair's car. The pair's cdr contains the count of all
;; key entries residing below that node. This count, contained in the pair's cdr, 
;; faclitates faster processing of the index by offset.
;;************************************************************************
pvars:(
	;;properties made persistent to index header     
	indexName				; string containing name of index
	uniqueSW				; true for unique, false for non-unique
	numericValuesSW			; true if only numeric values may be stored in index
	numKeys					; number of key entries
	numComponents			; number of key components - This allows us to differentiate between a key that is a vector
							; and a key that is supplied as a vector but is really a vector of component keys. This has impact
							; in a variety of query operations. numComponents corrosponds to the number of levels in the 
							; directory hierarchy.
	numValues				; number of value entries 
	rootNodeKey			; frameID of root node
	
	;; non persistent properties of index
	rootNode				; The top level index directory
	repository				; repository object the index is stored in
	frameID					; frameID of header of index
	snapshotSW				; Index is a snapshot - no update to original index allowed
	myObjectState			; current state of object; #void if uninitialized, true if active, false if closed
	mySelf					; a reference to the current object instance
	openedByName			; true if index was opened by name, false if opened by frameID
	
	; public functions
	clear					; delete all keys from the index      
	close					; save and close the index, return frameID of index header
	delete                  ; delete a key
	drop					; Drop an index from the repository
	init					; initialize index header information (usually called from new)
	insert					; insert new entry in index
	isMember 				; determine if a supplied key is in the index 
	length					; find the number of entries in the index
	member					; get the index of the first entry matching a supplied key
	new						; create a new index object. Open the index if it exists. Create new index if it does not exist.		
	open					; open an existing index Lambda by loading its header from disk (usually called from new)
	ref1					; called when index is referenced with one argument: index[x]
	ref2					; called when index is referenced with two arguments: index[x y]
	ref3   					; called when index is referenced with three arguments: index[x y z]
	refAttributes			; get a vector containing all keys from the index.
	refValues				; get a vector containing the index values. (key match can be supplied)
	save					; force save of index -- does not close index, returns frameID of index header
	set1					; called when index is set with one argument: (setq index[x] a)
	set2                    ; called when index is set with two arguments: (setq index[x y] a)
	set3					; called when index is set with three arguments: (setq index[x y z] a)
	update					; update a value in a non-unique index: (setq myindex.update key oldvalue newvalue)
	; private functions
	__beginTrans
	__commitTrans
	__errorState			; Error handler for error state violation -- see myObjectState property
	__rollbackTrans
	__collectValues
	__collectKeys 
	__getKeyByOffset
	__getValueByOffset
	)
;;********************************************************
;;********************************************************
(defun clear(...)
;; clear all keys from index
;; Returns true
	vars:(nmClear dir)
	(if (not myObjectState) (__errorState "clear"))
	
	(if (<> rootNode #void)
		(begin
		(setq dir (car rootNode))
		(^resize dir 0)
		(setCdr rootNode 0)
		end)
	);if
	(setq numKeys 0)
	(setq numValues 0)
true); close
;;********************************************************
;;********************************************************
(defun close(...)
;; Save and close the index
;; Optional Arguments
;; noSave: -- close the index but do not save changes to its content
;; Note that noSave is not available except on an index opened with the memory: option 
;; Returns frameID of index Header
	vars:(nmClose arg numArgs i noSaveSW)
	(if (not myObjectState) (__errorState "close"))

	(setq numArgs (argCount))
	(if (> numArgs 0) 
		(if (= (argFetch 0) noSave:) 
			(setq noSaveSW true)
		else
			(error "index:close: Argument supplied is not noSave:"))
	);if
		
	(if noSaveSW
		(begin
		(if (not memorySW)
			(error "index.close: Attempt to close with noSave: on an index not opened with memory: option.")
		);if
		end)
	else ; save the current index content to the repository
		(save)
	);if
	
	(setq myObjectState false); set closed state
	(setq rootNode #void)

	frameID	;return frameID
); close
;;********************************************************
;;********************************************************
(defun delete (x ...)
;; Deletes specified keys
;; Arguments
;;	x			- vector of key components 
;; Optional Arguments
;; oldvalue			- value to delete from non-unique index
;; Returns - reference to index

	vars:(nmDelete i c
		dir
		node
		value
		keys
		numArgs
		numComponentsPassed 
		valueSW 
		oldValue)

	(if (not myObjectState) (__errorState "delete"))
	(setq numArgs (argCount))
	;; Set defaults and collect arguments
	(setq valueSW false)
	(setq oldValue #void)
	(if (> numArgs 2) (error "index.delete: more than two arguments passed"))
	(if (= numArgs 2) 
		(begin 
		(setq valueSW true) 
		(setq oldValue (argFetch 1))))

	(if (= numComponents 1)
		(begin ;single component key
		(setq keys (^new Vector: 1 x))
		(setq numComponentsPassed 1)
		end)
	else
		(begin ;composite key
		(if (not (isVector x))
			(error "index.delete: key argument is not a vector of key components"))
		(setq keys (^copy x))
		(setq numComponentsPassed (^length keys))
		;make sure oldValue was not passed with partial key on non-unique index
		(if (<> numComponentsPassed numComponents)
			(error "index.delete: incomplete key passed to delete."))
		end)
	);if

	;Descend through each level of index
	(setq value rootNode)
 	(loop for c from 0 until numComponents do
		(setq dir (car value))
 		;look for match
 		(setq i (^member keys[c] dir))
 		(if (= i false)
 			(error "index.delete: key argument invalid")
 			(setq value dir[i 1]))
 	);

	(if uniqueSW
		(begin
		(^delete dir i)
		(setq numKeys (- numKeys 1))
		end)
	else ; handle non unique indices
		(begin
		(if (not (isVector value))
			(error "index.delete: non-unqiue index corruption."))
		(setq j (^member oldValue value)) ;find old value in vector of values
		(if (<> j false)
			(^delete value j) ;delete old value from vector
			(error "index.delete: oldvalue does not exist"))
		(if (= 0 (^length value)) ;is the vector not empty?
			(begin
			(^delete dir i) ;if so then delete the key entry as well
			(setq numKeys (- numKeys 1))
			end)
		);if
		end)
	);if	
	(setq numValues (- numValues 1))
	mySelf)
;;********************************************************	
;;********************************************************
(defun drop ()
;; delete the index
	vars:(nmDrop) 
	(if (not myObjectState) (__errorState "drop"))
	(if (<> rootNodeKey #void)
		(setq rootNodeKey (setq repository[frame: rootNodeKey] #void)))

	(if openedByName
		(setq frameID (setq repository[indexName] #void))
		(begin
		(if (<> frameID #void)
			(setq frameID (setq repository[frame: frameID] #void)))
		end)
	);if

	(setq myObjectState false); set object state to closed
	(setq rootNode #void)
true)
;;********************************************************	
;;********************************************************
;;********************************************************	
;;********************************************************
(defun init (argIndexKey argRepos ...)
;; see new for definition of arguments
	vars:(nmInit arg numArgs i indexHeader transLambdas openOption countOfNumericArgs createSW)

	;; Make sure index object is not already initialized
	(if (<> myObjectState #void) ; myObjectState is void if object is not initialized
		(if (= myObjectState true)
			(error "index.init; Attempt to initialize index object a second time")
		else ;myObjectState is false
			(error "index.init; Attempt to initialize a closed index object")
		);if
	);if

	(setq myObjectState true)
	(setq mySelf (myself)); set local reference to this instance of the object

	;;Check arguments
	(if (not (or (isSymbol argIndexKey) (isString argIndexKey) (isNumber argIndexKey) (= argIndexKey #void)))
		(error "index.init; first argument is not a index key (string: or symbol: or FrameID or #void)"))

	(if (not (isType ObjectRepository: argRepos)) 
		(error "index.init; second argument is not a repository object")
		(setq repository argRepos))
		
	;Set defaults for optional arguments 
	(setq numComponents 1)
	(setq createSW false); createSW is a flag indicating that create: was passed
	(setq memorySW true);
	(setq uniqeSW false)
	(setq snapshotSW false)
	(setq transLambdas #void)
	(setq numericValuesSW false)
	(setq countOfNumericArgs 0)

	(setq numArgs (argCount))

;(display "index.init ")
;(loop for i from 0 until numArgs do (display (argFetch i) " "))
;(display _eol)
	
	(loop for i from 2 until numArgs do
		(setq arg (argFetch i)) 
		(cond
		((= arg create:) (setq createSW true))

		((= arg unique:) 
			(if createSW
				(setq uniqueSW true)
				(error "index.init: unique: argument passed without create: option"))
		);unique

		((= arg numvalues:) 
			(if createSW
				(setq numericValuesSW true)
				(error "index.init: numvalues: argument passed without create: option"))
		);numvalues

		((isString arg) 
			(begin 
			;(setq keyCompare arg) 
			(error "index.init: keyCompare argument passed - not yet implemented")
			))

		((= arg memory:) (setq memorySW true))

		((= arg snapshot:) (setq snapshotSW true))

		((isStructure arg) (setq transLambdas arg))

		((isNumber arg) 
			(begin
			(if createSW 
				(cond
				((= countOfNumericArgs 0) (setq numComponents arg))
				((> countOfNumericArgs 1) (error "index.init: more than one numeric arguments passed"))
				)
			else ; allowed open options 
				(cond
				(true (error "index.init: no numeric arguments allowed except on create"))
				)
			);if
			(setq countOfNumericArgs (+ countOfNumericArgs 1))
			end)
		);isNumber
		
		(true (error "index.init; Bad optional argument passed "))
		);cond
	);i

	;Merge transaction Lambdas
	(if (<> transLambdas #void) (objectToStructure  mySelf.Pv transLambdas))

	; The argIndexKey is either an index name or frameID. This determines
	; how we will access the index in the repository.
	(if (isString argIndexKey) (setq argIndexKey (symbol argIndexKey)))

	(__beginTrans repository)

	(setq openedByName (isSymbol argIndexKey))
	(if (isSymbol argIndexKey) ;; look for index in repository by name
		(setq indexHeader argRepos[argIndexKey])
	else ;; look for index in repository by frameID
		(setq indexHeader argRepos[frame: argIndexKey])
	);
	(__commitTrans repository)

	;Merge index header values into index object if it exists
	(if (<> indexHeader #void)
		(begin 
		(if openOption (error "index.init: Index exists, invalid option passed. One of; unique:, numvalues: or keyCompare string"))
		(objectToStructure mySelf.Pv indexHeader)
		end)
	else ;initialize header values in object's pvars
		(begin
		(if (isSymbol argIndexKey)
			(setq indexName argIndexKey)
			(setq indexName #void))
		(setq numKeys 0)
		(setq numValues 0)
		(setq rootNodeKey #void)
		end)
	);if

	;Load entire index into memory if memory: flag passed
	(if memorySW 
		(begin
		; Load exiting index into memory
		(if (<> rootNodeKey #void)
			(begin

			(__beginTrans repository)
			(setq rootNode repository[frame: rootNodeKey])
			(if (= rootNode #void) (error "index.init: root node not found"))
			(__commitTrans repository)
		 	end)
		else ; create new empty rootNode directory
			(setq rootNode (cons (^new Directory:) 0)) 
		);if
		end)
	);if

;(writeln "nodes[0]=" nodes[0])

;(writeln "INIT argIndexKey=" argIndexKey " uniqueSW:"unqiueSW " numericValuesSW:"numericValuesSW " numKeys:"numKeys " numComponents:"numComponents " rootNodeKey:"rootNodeKey)

	;Copy index if snapshot: flag passed and we are paging to disk
	(if (and snapshotSW (not memorySW))
		(writeln "error: index.init: snapshot: not yet implemented on non-memory index opens"));if
	
mySelf)
;;********************************************************
;;********************************************************
(defun __insert(key argValue)
;; insert new key and value into index, returns reference to index object
	vars:(nm__Insert c i len node nodes dir keys bottom value)
	(if (not myObjectState) (__errorState "insertKey"))
    ;test for valid arguments
	(if (and numericValuesSW (not (isNumber value))) 
		(error "index.insert: Attempt to insert non-numeric value into numeric value index"))

	(if (= numComponents 1) 
		(begin
		(setq len 1) 
		(setq keys (^new Vector: 1 key))
		end)
	else
		(begin
		(setq len (^length key))
		(setq keys key)
		(if (<> len numComponents)
			(error "index.__insert: key argument has too few components for index key."))
		end)
	);if

  	;Descend through the index hierarchy, finding the appropriate key component at each level
  	(setq nodes (^new Vector: numComponents))
  	(setq bottom (- len 1))
  	(setq value rootNode)
	(loop for c from 0 until len do
		(setq node value)
		(setq dir (^car node))
		(setq nodes[c] node); nodes will contain the descent path to our final insertion node 
		;find key in current node
		(setq i (^member keys[c] dir))
		(if (= i false) 
			(if (= c bottom)
				(begin ;perform final insertion for new key
				(if uniqueSW
					(setq dir[keys[c]] argValue) ;make assignment of key value pair
				else ; handle non-unique index insert
					(begin
					(if numericValuesSW
						(setq value (^new Vector: number:))
						(setq value (^new Vector:)))
					(^binaryInsert value argValue)
					(setq dir[keys[c]] value)
					end) 
				);if
				;update the key count of each directory in the nodes descent list
				(setq numKeys (+ numKeys 1))
				(setq numValues (+ numValues 1))
				(loop for j from 0 until len do
					(^setCdr nodes[j] (+ (^cdr nodes[j]) 1))
;					(writeln "nodes["j"] cdr=" (^cdr nodes[j]))
				);j
				end)
			else
				(begin ;extend index hierarchy by one level
				(setq value (^cons (^new Directory:) 0)); create new node
				(setq dir[keys[c]] value)
				end)
			);if
		else ;key component found
			(begin ; existing key component entry found
			(if (= c bottom)
				(begin
				(if uniqueSW
					(error "index.insert: key/value pair already exists"))
				(setq value dir[i 1]); get existing vector of values
				(setq valLen (^length value))
				(^binaryInsert value argValue)
				(setq numValues (+ numValues (- (^length value) valLen)))
				end)
			else
				(setq value dir[i 1]); value now contains next node
			);if
			end)
		);if
	);i

	mySelf)
;;********************************************************
;;********************************************************
(defun isMember(key)
	;; args 
	;;	key	- key if single component index. Vector of keys if multi-component index
	vars:(nmIsMember i c keys value node dir)
	(if (not myObjectState) (__errorState "isMember"))

	;convert single key argument to vector
	(if (= numComponents 1)
		(setq keys (^new Vector: 1 key))
		(setq keys key))

	(if (<> (^length keys) numComponents)
		(error "index.isMember: key argument does not contain enough key components"))

	;descend through index hierarchy matching keys
	(setq len (^length keys))
	(setq value rootNode)
 	(loop for c from 0 until len do
 		(setq node value)
 		(setq dir (^car node))
 		(setq i (^member keys[c] dir))
 		(if (= i false) 
 			(return false)
 			(setq value dir[i 1])); assign value to next node
 	);	
	true)
;;********************************************************
;;********************************************************
(defun length()
	vars:(nmLength)
	(if (not myObjectState) (__errorState "length"))
	numKeys)

;;********************************************************
;;********************************************************
(defun member(key)
	pvars:(offset)
	vars:(nmMember i c j keys value node dir offset)
	(if (not myObjectState) (__errorState "member"))
	;convert single key argument to vector
	(if (= numComponents 1)
		(setq keys (^new Vector: 1 key))
		(setq keys key))

	(if (<> (^length keys) numComponents)
		(error "index.isMember: key argument does not contain enough key components"))

	;descend through each level of the tree calculating the offset value as you 
	;go through each level.
	(setq value rootNode)
	(setq offset 0)
    (loop for c from 0 until numComponents do
    	(setq node value)
    	(setq dir (^car node))
    	(setq i (^member keys[c] dir))
    	(if (= i false)
    		(return false))
    	(if (< (+ c 1) numComponents)
    		(begin ;calculate additive offset for current level
	    	(loop for j from 0 until i do
	    		(setq offset (+ offset (^cdr dir[j 1])))
	    	);j
	    	(setq value dir[i 1]) ; value now contains next level node
	    	end)
	    else ; we hit bottom so just add the position in this level to offset and return it
	    	(return (+ offset i))
	    );if
    );c

	(error "index.member: unexpected error")
	)
;;********************************************************
;;********************************************************
(defun new(...)
	vars:(nmNew i args numArgs)
	(setq numArgs (argCount))
	(setq args (^new Vector: numArgs))
	(loop for i from 0 until numArgs do
		(setq args[i] (argFetch i))
	); i
	(apply init args)
true)
;;********************************************************
;;********************************************************
;;********************************************************
;;********************************************************
(defun ref1(key)
;; ref1 is called when object is referenced as object.x or object[x]
;; returns Value from index
	vars:(nmRef1 i keys value numPassed node dir result)
	(if (not myObjectState) (__errorState "ref1"))

	;convert single key argument to vector
	(if (= numComponents 1)
		(setq keys (^new Vector: 1 key))
		(setq keys key))
  	
	;if numPassed is less than numComponents, then a partial search
	;has been requested.
  	(setq numPassed (^length keys))

	;descend through index hierarchy matching keys
	(setq value rootNode)
 	(loop for c from 0 until numPassed do
 		(setq node value)
 		(setq dir (^car node))
 		(setq i (^member keys[c] dir))
 		(if (= i false) 
 			(return #void)
 			(setq value dir[i 1]))
 	);	
	
  	;if partial search or non-unique index then create result vector and fill it from current node
  	; down through the rest of the tree
  	(if (< numPassed numComponents)
  		(setq result (__collectValues value c #void))
  	else
  		(setq result value))

	result);ref1  
	
;;********************************************************
;;********************************************************
(defun ref2(x y)
;; ref2 is called when object is referenced in one of the following forms
;; object[offset 1]	; return the value associated with index entry at offset
;; object[offset 0] ; return the key associated with index entry at offset
    vars:(nmRef2)
   
	(if (not myObjectState) (__errorState "ref2"))

	(cond
	((= y 0)(return (__getKeyByOffset x)));
	((= y 1)(return (__getValueByOffset x)));
	(true	(error (append "index.ref2: Second argument must be 0 or 1. " y " was passed.")));
	);cond

BADIDXORKEY::
	(error "index.ref2: badIdxOrKey")
)
	
;;********************************************************
;;********************************************************
(defun ref3(x y z)
;;
	vars:(nmRef3) 
	(if (not myObjectState) (__errorState "ref3"))
	(error "index.ref3: Invalid reference - three arguments not supported.")	
	);ref3
;;********************************************************
;;********************************************************
(defun refAttributes(...)
	vars:(numArgs i c node dir value keys result)
	(if (not myObjectState) (__errorState "refAttributes"))
	(setq numArgs (argCount))

	;convert single key argument to vector
	(if (> numArgs 0)
		(if (= numComponents 1)
			(setq keys (^new Vector: 1 (argFetch 0)))
			(setq keys (argFetch 0)))
	else
		(setq keys (^new Vector:))
	);if
	
	;if numPassed is less than numComponents, then a partial search
	;has been requested.
  	(setq numPassed (^length keys))

	;descend through index hierarchy matching keys
	(setq value rootNode)
 	(loop for c from 0 until numPassed do
 		(setq node value)
 		(setq dir (^car node))
 		(setq i (^member keys[c] dir))
 		(if (= i false) 
 			(return #void)
 			(setq value dir[i 1]))
 	);	
	
  	;if partial search or non-unique index then create result vector and fill it from current node
  	; down through the rest of the tree
  	(if (< numPassed numComponents)
  		(setq result (__collectKeys value keys #void))
  	else
  		(setq result dir[i 0]))

	result)
;;********************************************************
;;********************************************************
(defun refValues(...)
	vars:(nmRefValues numArgs i c keys node dir value numPassed numPassedLessOne result)
	(if (not myObjectState) (__errorState "refValues"))
	(setq numArgs (argCount))

	;convert single key argument to vector
	(if (> numArgs 0)
		(if (= numComponents 1)
			(setq keys (^new Vector: 1 (argFetch 0)))
			(setq keys (argFetch 0)))
	else
		(setq keys (^new Vector:))
	);if

	;if numPassed is less than numComponents, then a partial search
	;has been requested.
  	(setq numPassed (^length keys))

	;descend through index hierarchy matching keys
	(setq value rootNode)
 	(loop for c from 0 until numPassed do
 		(setq node value)
 		(setq dir (^car node))
 		(setq i (^member keys[c] dir))
 		(if (= i false) 
 			(return #void)
 			(setq value dir[i 1]))
 	);	
	
  	;if partial search or non-unique index then create result vector and fill it from current node
  	; down through the rest of the tree
  	(if (< numPassed numComponents)
  		(setq result (__collectValues value numPassed #void))
  	else
  		(begin
  		(if uniqueSW
  			(begin
  			(setq result (^new Vector: 1))
  			(setq result[0] value)
  			end)
  		else
  			(setq result value)
  		);if
  		end)
  	);if
	
result)
;;********************************************************
;;********************************************************
(defun save () 
;; Save index -- does not close index, returns frameID of index header
	vars:(nmSave indexHeader)
	(if (not myObjectState) (__errorState "save"))

	;; Save root node -- for now this is our only node
	(setq rootNodeKey (setq repository[frame: rootNodeKey] rootNode))

	;; Save header
	(setq indexHeader (^new Structure:
		indexName: 			indexName
		uniqueSW: 			uniqueSW
		numericValuesSW: 	numericValuesSW
		numKeys:			numKeys 
		numValues:			numValues
		numComponents:		numComponents
		rootNodeKey:		rootNodeKey
		))
	(if openedByName
		(setq frameID (setq repository[indexName] indexHeader)) 
		(setq frameID (setq repository[frame: frameID] indexHeader)))

;(writeln "SAVE frameID="frameID "indexName:"indexName " uniqueSW:"unqiueSW " numericValuesSW:"numericValuesSW " numKeys:"numKeys " numComponents:"numComponents " rootNodeKey:"rootNodeKey)
	
	frameID); save
	
;;********************************************************
;;********************************************************
(defun set1(x value)
	vars:(nmSet1)
	(if (not myObjectState) (__errorState "set1"))
	(__insert x value)
	value)
;;********************************************************
;;********************************************************
(defun set2()
	vars:(nmSet2)
	(if (not myObjectState) (__errorState "set2"))
	(error "index.set2: set2 not implemented")
	false)
;;********************************************************
;;********************************************************
(defun set3()
	vars:(nmSet3)
	(if (not myObjectState) (__errorState "set3"))
	(error "index.set3: set3 not implemented")
	false)

;;********************************************************
;;*********************************************************
(defun update(key oldvalue newvalue)
	vars:(nmUpdate)
	(if (not myObjectState) (__errorState "update"))
	(if uniqueSW (error "index.update: update called on unique index."))
	(delete key oldvalue)
	(__insert key newvalue)
true)

;;********************************************************
;;*********************************************************
(defun __collectValues(node depth result)
;; recursively collect values in the index from the node specified
;; args:
;; 	node 	- current directory node in tree
;;	depth	- depth of current directory in tree
;;	result	- varaible holding result vector
;; returns
;;	Vector of values from index
	vars:(nm__CollectValues i j k numVals dir value len)
	(if (= result #void) (setq result (^new Vector:)))
	(setq dir (^car node))
	(setq len (^length dir))
	(loop for i from 0 until len do
		(if (= (+ depth 1) numComponents)
			(begin
			(if uniqueSW
				(setq result[(^length result)] dir[i 1])
			else
				(begin
				(setq value dir[i 1])
				(setq numVals (^length value))
				(setq k (^length result))
				(^resize result (+ k numVals))
				(loop for j from 0 until numVals do
					(setq result[k] value[j])
					(setq k (+ k 1))
				);j
				end)
			);if
			end)
		else
			(setq result (__collectValues dir[i 1] (+ depth 1) result))
		);if
	);i	
result)

;;********************************************************
;;*********************************************************
(defun __collectKeys(node prefix result)
;; recursively collect complete keys in the index from the node specified
;; args:
;; 	node 	- current directory node in tree
;;	depth	- depth of current directory in tree
;;  prefix 	- a vector of key components collected during descent to the current node.
;;	result	- varaible holding result vector 
;; returns
;;	Vector of values from index
	vars:(nm__CollectKeys i j k numVals value dir len keyEntry depth)
	(if (= result #void) (setq result (^new Vector:)))
	(setq dir (^car node))
	(setq len (^length dir))
	(setq depth (^length prefix))
	(loop for i from 0 until len do
		(if (> numComponents 1)
			(begin
			(setq keyEntry (^copy prefix))
			(setq keyEntry[(^length keyEntry)] (copy dir[i 0]))
			end)
		else
			(setq keyEntry (copy dir[i 0]))
		);if
		
		(if (= (+ depth 1) numComponents)
			(setq result[(^length result)] keyEntry)
		else
			(setq result (__collectKeys dir[i 1] keyEntry result))
		);if
	);i	
result)


;;********************************************************
;;*********************************************************
(defun __getValueByOffset(x)
	pvars:(nm__GetValueByOffset value y __findXValue)

	(defun __findXValue(node x level)
	;recursively find x position in the tree
		vars:(nm__FindXValue i len dir subdir count)
		(setq dir (^car node))
		(setq len (^length dir))
		(if (= (+ level 1) numComponents)
			(begin ;we are at the bottom of the tree
			(setq i (- x y))
			(if (>= i len) 
				(error "index.__getValueByOffset: offset value out of range"))
			(setq value dir[i 1])
			(return true)
			end)
		else
			(loop for i from 0 until len do
				(setq subdir dir[i 1])
				(setq count (^cdr subdir))
				(if (< x (+ y count))
					(begin
					(if (__findXValue subdir x (+ level 1))
						(return true))
					end)
				else
					(setq y (+ y count))
				);if
			);i
		);if

	false)

	(setq y 0)
	(__findXValue rootNode x 0)

value)

;;********************************************************
;;*********************************************************
(defun __getKeyByOffset(x)
	pvars:(nm__GetKeyByOffset keys nodeFound key y __findXKey)

	(defun __findXKey(node x level)
	;recursively find x position in the tree
		vars:(nm__FindXKey i len subNode dir count)
		(setq dir (^car node))
		(setq len (^length dir))
		(if (= (+ level 1) numComponents)
			(begin
			(setq i (- x y))
			(if (>= i len) 
				(error "index.__getValueByOffset: offset value out of range"))
			(setq keys[level] dir[i 0])
			(return true)
			end)
		else
			(loop for i from 0 until len do
				(setq subNode dir[i 1])
				(setq count (^cdr subNode))
				(if (< x (+ y count))
					(begin
					(setq keys[level] dir[i 0])
					(if (__findXKey subNode x (+ level 1)) 
						(return true))
					end)
				else
					(setq y (+ y count))
				);if
			);i
		);if

	false)

	(setq keys (^new Vector: numComponents))
	(setq y 0)
	(__findXKey rootNode x 0)

keys)

;;********************************************************
;;*********************************************************
(defun __beginTrans (argRepo)
	(beginTransaction argRepo)
	1);; end of beginTrans

;;********************************************************
;;*********************************************************
(defun __commitTrans (argRepo)
 	(commitTransaction argRepo)
	0);;end of commitTrans

;;********************************************************
;;*********************************************************
(defun __errorState(functionName)
;; Summary:  Issue error state error message
	(if (= myObjectState #void)
		(error (append "index." functionName "; called on index object not yet initialized."))
		(error (append "index." functionName "; called on close index object."))
	)
	true)

;;********************************************************
;;*********************************************************
(defun __rollbackTrans (argRepo)
	(abortTransaction argRepo)
	0);; end of rollbackTrans

;;********************************************************
;;*********************************************************
(defun __showTree ()
	vars:(c nodes len)

	(defun __showTreeNode(node level)
		vars:(len dir count i)
		(setq dir (car node))
		(setq len (^length dir))
		(setq count (cdr node))
		(writeln (rept "  " level) "Count=" count)
		(loop for i from 0 until len do
			(display (rept "  " level) "key[" level "]=" dir[i 0] )
			(if (= (+ level 1) numComponents) (display " " dir[i 1]))
			(display _eol)
			(if (< (+ level 1) numComponents)
				(__showTreeNode dir[i 1] (+ level 1)))
		);i 
	
	true)
	
	(__showTreeNode rootNode 0)

	true);; end of rollbackTrans

;**************************************************
;************** main body of index ****************
;**************************************************
 
true)                                                                        


(deforphan index:selfTest()
;**************************************************
;******** selfTest of index Lambda *****************
;**************************************************
vars:(repos myindex i entry)
(setq repos (new ObjectRepository: "indexSelfTestTempRepos.db"))
(clear repos)   

(writeln "********* Test single component key indexing ****************")
(setq myindex (new index myindex: repos create: unique: memory:))
;(myindex.Pv.init )
(setq myindex["a"] 1)
(writeln "myindex['a']=" myindex["a"])
(writeln "myindex[0 0]=" myindex[0 0])
(writeln "myindex[0 1]=" myindex[0 1])

(setq myindex["ab"] 2)
(writeln "myindex['ab']=" myindex["ab"])
(writeln "myindex[1 0]=" myindex[1 0])
(writeln "myindex[1 1]=" myindex[1 1])

(writeln "myindex.Pv.member 'ab'=" (myindex.Pv.member 'ab'))
(writeln "myindex['a']=" myindex["a"])
(writeln "refValues=" (myindex.Pv.refValues))
(writeln "refAttributes=" (myindex.Pv.refAttributes)) 
(writeln "numValues=" myindex.Pv.numValues)
(writeln "numKeys=" myindex.Pv.numKeys)

(myindex.Pv.close)

(writeln "**************************************")
(setq myindex #void)
(setq myindex (new index myindex: repos memory:)) 
;(myindex.Pv.init )
(writeln "myindex['a']=" myindex["a"])
(writeln "myindex[0 0]=" myindex[0 0])
(writeln "myindex[0 1]=" myindex[0 1])
(writeln "myindex['ab']=" myindex["ab"])
(writeln "myindex[1 0]=" myindex[1 0])
(writeln "myindex[1 1]=" myindex[1 1])
(writeln "myindex['a']=" myindex["a"])
(writeln "refValues=" (myindex.Pv.refValues))
(writeln "refAttributes=" (myindex.Pv.refAttributes))

(writeln "numValues=" myindex.Pv.numValues)
(writeln "numKeys=" myindex.Pv.numKeys)

(writeln "myindex.Pv.delete 'ab'")
(myindex.Pv.delete "ab")
(myindex.Pv.__showTree)

(writeln "numValues=" myindex.Pv.numValues)
(writeln "numKeys=" myindex.Pv.numKeys)

(myindex.Pv.drop)


(writeln "********* Test composite key indexing ****************")

(setq myindex (new index myindex: repos create: 2 unique: memory:))
;(myindex.Pv.init )
(setq myindex[#("a" 1)] 1)
(writeln "myindex[#('a' 1)]=" myindex[#("a" 1)])
(writeln "myindex[0 0]=" myindex[0 0])
(writeln "myindex[0 1]=" myindex[0 1])

(setq myindex[#("a" 2)] 2)
(writeln "myindex[#('a' 2)]=" myindex[#("a" 2)])
(writeln "myindex[1 0]=" myindex[1 0])
(writeln "myindex[1 1]=" myindex[1 1])


(setq myindex[#("b" 1)] 3)
(writeln "myindex[#('b' 1)]=" myindex[#("b" 1)])
;(myindex.Pv.__showTree)

(writeln "myindex[2 0]=" myindex[2 0])
(writeln "myindex[2 1]=" myindex[2 1])

(writeln "myindex[0 0]=" myindex[0 0])
(writeln "myindex[0 1]=" myindex[0 1])

(writeln "myindex[1 0]=" myindex[1 0])
(writeln "myindex[1 1]=" myindex[1 1])

(writeln "myindex.Pv.member #('b' 1)=" (myindex.Pv.member #('b' 1)))
(writeln "myindex[#('a' 1)]=" myindex[#("a" 1)])

(writeln "refValues=" (myindex.Pv.refValues))
(writeln "refValues #('a')=" (myindex.Pv.refValues #("a")))
(writeln "refValues #('a' 1)=" (myindex.Pv.refValues #("a" 1)))

(writeln "refAttributes=" (myindex.Pv.refAttributes)) 
(writeln "refAttributes #('a')=" (myindex.Pv.refAttributes #("a")))

(writeln "numValues=" myindex.Pv.numValues)
(writeln "numKeys=" myindex.Pv.numKeys)

(writeln "myindex.Pv.delete #('a' 1)")
(myindex.Pv.delete #("a" 1))
(myindex.Pv.__showTree)

(writeln "numValues=" myindex.Pv.numValues)
(writeln "numKeys=" myindex.Pv.numKeys)



(myindex.Pv.drop)


(writeln "********* Test non-unique indexing ****************")
(setq myindex (new index  myindex: repos create: memory:)) 
;(myindex.Pv.init)
(setq myindex["a"] 1)
(setq myindex["a"] 2)
(writeln "myindex['a']=" myindex["a"]) 
(writeln "myindex[0 0]=" myindex[0 0])
(writeln "myindex[0 1]=" myindex[0 1])

(setq myindex["ab"] 3)
(writeln "myindex['ab']=" myindex["ab"])
(writeln "myindex[1 0]=" myindex[1 0])
(writeln "myindex[1 1]=" myindex[1 1])

(writeln "myindex['a']=" myindex["a"])
(writeln "refValues=" (myindex.Pv.refValues))
(writeln "refAttributes=" (myindex.Pv.refAttributes))

(writeln "numValues=" myindex.Pv.numValues)
(writeln "numKeys=" myindex.Pv.numKeys)

(writeln "myindex.Pv.delete 'a' 2")
(myindex.Pv.delete "a" 2)
(myindex.Pv.__showTree)
(writeln "numValues=" myindex.Pv.numValues)
(writeln "numKeys=" myindex.Pv.numKeys)


(writeln "myindex.Pv.delete 'a' 1")
(myindex.Pv.delete "a" 1)
(myindex.Pv.__showTree)
(writeln "numValues=" myindex.Pv.numValues)
(writeln "numKeys=" myindex.Pv.numKeys)

(writeln "myindex.Pv.delete 'ab' 3")
(myindex.Pv.delete "ab" 3)
(myindex.Pv.__showTree)
(writeln "numValues=" myindex.Pv.numValues)
(writeln "numKeys=" myindex.Pv.numKeys)

(setq myindex["a"] 1)
(setq myindex["a"] 2)
(writeln "myindex['a']=" myindex["a"]) 
(writeln "myindex[0 0]=" myindex[0 0])
(writeln "myindex[0 1]=" myindex[0 1])


(myindex.Pv.drop)

(setq repos #void)
true)




