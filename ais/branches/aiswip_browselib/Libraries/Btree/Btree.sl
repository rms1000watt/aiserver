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

;;**EXPORTKEY**:btree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; btree Database Lambda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Tim May
;;	
;; Background: See SQLITE 2.8 at sqlite.org
;;
;; This Lambda implements an in-core database providing a somewhat modified 
;; version of the sqlite btree api expected by the sqlite VBE. This Lambda
;; is an experiment to exercise the smtbase engine's vm.. instruction set
;; and the JIT to create a very high speed in-core database to support the
;; development of our own sqlite VBE like virtual machine. Later on, this
;; Lambda may be expanded to include a file based b+tree implementation if
;; our testing and implementation of the SQL virtual machine pans out.
;;
;; The btree Lambda creates and manages a database structure. 
;; In the future this database would relate to a single file on disk. In this
;; memory database implmentation this Lambda has no disk representation 
;; unless you store the database structure object closure in a repository.
;;
;; Create new btree database structure like this:
;;	(setq mydb (btree memory:))
;;
;; Each database structure contains a directory of tables. Each table is identified
;; by an integer key value (iTable). A table is either a sql table or an 
;; index. 
;;
;; Create new tables like this:
;;	(setq iTable (btree.createTable mydb))
;;
;; Access to data in tables is performed through cursors. Each table may have
;; one or more active cursors. A cursor is a structure inside the database structure.
;;
;; Create a new cursor like this:
;;	(setq myCursor (btree.createCursor mydb iTable wrFlag))
;;
;; Because btree is designed to support an SQL database engine the names of
;; tables and their field specifications (the database schema) are usually 
;; stored in a master table that resides in the database structure as table 0. 
;; This is not a function performed by the btree Lambda but rather something the
;; "next higher" level agency will do to implement SQL functionality.
;;
;; This Lambda is named btree because it implements a red black binary tree
;; (rbtree) memory store. The eventual disk based version of this Lambda 
;; may implement a B+tree or B*tree as these are more efficient for disk
;; based stores.
;;
;; To better understand btree you should review and execute the brtee.selfTest 
;; Lambda. selfTest is designed to fully exercise the btree Lambda and has
;; a tutorial level of comments.
;;
;; Note that this Lambda separates the database structure from the btree Lambda complex.
;; If you store the database structure into a repository it will not contain the 
;; btree Lambda. This approach was taken to minimize the footprint of multiple databases
;; in memory and those saved to disk. This approach also ensures that future access to
;; a file based representation of the database store is well understood. This architecture
;; parallels the C implementation of sqlite more closely as well.
;;
(defun btree(fileName)
	pvars:(
	;Public Child Lambdas
		;General Operations	
		close				;; (mydb) Returns true or false if close failed
		setCacheSize		;; (mydb sz) Returns true or false if call failed
		setSafetyLevel		;; (sl) Returns true or false if call failed
		beginTrans			;; (mydb) Returns true or false if call failed
		commit				;; (mydb) Returns true or false if call failed
		rollback			;; (mydb) Returns true or false if call failed
		beginCkpt			;; (mydb) Returns true or false if call failed
		commitCkpt			;; (mydb) Returns true or false if call failed
		rollbackCkpt		;; (mydb) Returns true or false if call failed

		;Table Operations
		createTable			;; (mydb) Returns tableIdx or false if call failed
		createIndex			;; (mydb) Returns tableIdx of false if call failed
		dropTable			;; (mydb iTable) true or false if call failed
		clearTable			;; (mydb iTable) true or false if call failed
		createCursor		;; (mydb iTable wrFlag) cursor or false if call failed

		;Meta Operations
		getMeta				;; (mydb aMeta) copy of metadata
		updateMeta			;; (mydb aMeta) true
		integrityCheck		;; (mydb aRoot nRoot) true - writelns errors found
		getFilename			;; (mydb) Returns filename
		copyFile			;; (mydb) copy of mydb structure

		;Cursor Operations
		; Note that mydb is not passed to these Lambdas because the cursor structure
		; has a reference to the mydb structure.
		moveto				;; (cursor pKey, nKey) Integer value or false if call failed
		delete				;; (cursor) true or false if call failed
		insert				;; (cursor pKey, nKey, pData, nData) true or false if call failed
		first				;; (cursor) 0 if no node, 1 if node found
		last				;; (cursor) 0 if no node, 1 if node found
		next				;; (cursor) 0 if next node found, 1 if no next node 
		previous			;; (cursor) 0 if prev node found, 1 if no prev node 
		keySize				;; (cursor) size of key or false if call failed
		key					;; (cursor offset, amt, zBuf) ????
		keyCompare			;; (cursor pKey, nKey, nIgnore) compare result or false if call failed
		dataSize			;; (cursor) data size or  false if call failed
		data				;; (cursor offset, amt, zBuf) ???
		closeCursor			;; (cursor) #void

		;Private Operations
		_checkReadLocks			;; 
		_keyCompare
		_rightRrotate
		_leftRotate
		_do_insert_balancing
		_do_delete_balancing
		_logRollbackOp
		_execute_rollback_list
		_check_redblack_tree
				
		;Templates and Constants
			;;Legal values for dbTemplate.transState.
			(TRANS_NONE           0)  ; No transaction is in progress
			(TRANS_INTRANSACTION  1)  ; A transaction is in progress
			(TRANS_INCHECKPOINT   2)  ; A checkpoint is in progress  
			(TRANS_ROLLBACK       3)  ; We are currently rolling back a checkpoint or transaction.

			(SQLITE_N_BTREE_META 10) ; size of metaData vector
			(dbTemplate
				#{	metaData: #void
					next_idx: 0
					tablesDir: #void
					isAnomoymous: true
					transState: false
			 		transRollback: #void
					checkRollback: #void
					checkRollbackTail: #void
				})
	
			(tableTemplate
				#{	cursorsDir: #void
					head: #void
				})
	
			;; Legal values for cursorTemplate.skip.
			(SKIP_NONE     0)  ; Always step the cursor
			(SKIP_NEXT     1)  ; The next sqliteRbtreeNext() is a no-op
			(SKIP_PREV     2)  ; The next sqliteRbtreePrevious() is a no-op
			(SKIP_INVALID  3)  ; Calls to Next() and Previous() are invalid
			
			(cursorTemplate
				#{	mydb: #void
					tableStruct: #void
					tableIdx: #void
					node: #void
					cursorsDir: #void
					skip: #void
					wrFlag: #void
				})

			;; Legal values for rollbackTemplate.op
			(ROLLBACK_INSERT 1) ; Insert a record
			(ROLLBACK_DELETE 2) ; Delete a record 
			(ROLLBACK_CREATE 3) ; Create a table
			(ROLLBACK_DROP   4) ; Drop a table

			(rollbackTemplate 
				#{ 	op: #void
					tableIdx: #void
					nKey: 0
					key: #void
					nData: 0
					data: #void
					next: #void
				})

			(nodeTemplate 
				#{	nKey: 0
					key: #void
					nData: 0
					data: #void
					isBlack: 0
					parent: #void
					left: #void
					right: #void
					blackHeight: 0 ; used only during the red-black integrity check
				})

		;Self Test Operations
		selfTest ;; Perform a complete selftest of btree Lambda

		); end pvars

	;; btree - return a new database structure
	;; example: (setq mydb (btree memory:))
	;;;;;;;;;;;;;;* MAIN CODE ;;;;;;;;;;*
	vars:(mydb) 
	(if (<> fileName memory:)
		(return (error "btree - this version of btree can only open as a memory: database")))
	(setq mydb (copy dbTemplate))
	(setq mydb.metaData (new Vector: number: SQLITE_N_BTREE_META))
	(setq mydb.tablesDir (new Directory:))
	
	;; Create a binary tree for the SQLITE_MASTER table at location 2
	(createTable mydb 2)

  	;; Set file type to 4; this is so that "attach ':memory:' as ...."  does not
	;; think that the database in uninitialised and refuse to attach
	(setq mydb.metaData[2] 4);

	(return mydb) ; This is a database structure ready to receive its schema contents

;; This routine checks all cursors that point to the same table
;; as cursor points to.  If any of those cursors were opened with
;; wrFlag==0 then this routine returns true.  If all
;; cursors point to the same table were opened with wrFlag==1
;; then this routine returns false.
;;
;; In addition to checking for read-locks (where a read-lock 
;; means a cursor opened with wrFlag==0) this routine also NULLs
;; out the myNode field of all other cursors.
;; This is necessary because an insert 
;; or delete might change or erase the node out from under
;; another cursor.
(defun _checkReadLocks(cursor)
	vars:(n N cur cursorsDir (locked false))
	(setq cursorsDir cursor.cursorsDir)
	(setq N (length cursorsDir))
	(loop for n from 0 until N do
		(setq cur cursorsDir[n 0])
		(if (and (= cur.wrFlag 0) (= locked false))
			(setq locked true))
		(setq cur.myNode #void)
	);n
	locked); end checkReadLocks

;; The key-compare function for the red-black trees. Returns as follows:
;; (key1 < key2)             -1
;; (key1 == key2)             0 
;; (key1 > key2)              1
(defun _key_compare( Key1 nKey1 Key2 nKey2)
  (compare Key1 Key2))

;; Perform the LEFT-rotate transformation on node X of tree pTree. This
;; transform is part of the red-black balancing code.
;;
;;        |                   |
;;        X                   Y
;;       / \                 / \
;;      a   Y               X   c
;;         / \             / \
;;        b   c           a   b
;;
;;      BEFORE              AFTER
(defun _leftRotate(table, nodeX)
	vars:(nodeY nodeB)
	(setq nodeY nodeX.right)
	(setq nodeB nodeY.left)
	(setq nodeY.parent nodeX.parent)
	(if (<> nodeX.parent #void)
		(if (= nodeX.parent.left nodeX)
			(setq nodeX.parent.left nodeY)
			(setq nodeX.parent.right nodeY)))
	(setq nodeY.left nodeX)
	(setq nodeX.parent nodeY)
	(setq nodeX.right nodeB)
	(if (<> nodeB #void) (setq nodeB.parent nodeX))
	(if (= table.head nodeX) (setq table.head nodeY))
true)

;; Perform the RIGHT-rotate transformation on node X of the table. This
;; transform is part of the red-black balancing code.
;;
;;        |                   |
;;        X                   Y
;;       / \                 / \
;;      Y   c               a   X
;;     / \                     / \
;;    a   b                   b   c
;;
;;      BEFORE              AFTER
(defun _rightRotate(table nodeX)
	vars:(nodeY nodeB)
	(setq nodeY nodeX.left)
	(setq nodeB nodeY.right)
	(setq nodeY.parent nodeX.parent)
	(if (<> nodeX.parent #void)
		(if (= nodeX.parent.left nodeX)
			(setq nodeX.parent.left nodeY)
			(setq nodeX.parent.right nodeY)))
	(setq nodeY.right nodeX)
	(setq nodeX.parent nodeY)
	(setq nodeX.left nodeB)
	(if (<> nodeB #void) (setq nodeB.parent nodeX))
	(if (= table.head nodeX) (setq table.head nodeY))
	true)

;; Check the following properties of the red-black tree:
;; (1) - If a node is red, both of it's children are black
;; (2) - Each path from a given node to a leaf (NULL) node passes thru the
;;       same number of black nodes 
;;
;; If there is a problem, writeln messages
(defun _check_redblack_tree(mydb)
	vars:(node prev_step leftHeight rightHeight)

	(setq prev_step 0)
	; 0 -> came from parent 
	; 1 -> came from left
	; 2 -> came from right

	(setq node mydb.head)
	(while (<> node #void)
		(cond
		((= prev_step 0) ; came from parent
			(if (<> node.left #void)
				(setq node node.left)
				(setq prev_step 1)		)
		)

		((= prev_step 1) ; came from left
			(if (<> node.right #void)
				(begin
				(setq node node.right)
				(setq prev_step 0))
			else
				(setq prev_step 2))
		)

		((= prev_step 2) ; came from right

			;; Check red-black property (1)
			(if (and (= node.isBlack 0) 
				(or (and (<> node.left #void) (= node.left.isBlack 0))
					(and (<> node.right #void) (= node.right.isBlack 0))))
				(begin
				(writeln "Red node with red child at " node)
				))

			;; Check red-black property (2)
			(setq leftHeight 0)
			(setq rightHeight 0)
			(if (<> node.left #void)
				(setq leftHeight (+ leftHeight node.left.blackHeight node.left.isBlack)))

			(if (<> node.right #void)
				(setq rightHeight (+ rightHeight node.right.blackHeight node.right.isBlack)))

			(if (<> leftHeight rightHeight)
				(writeln "Different black-heights at " node))

			(setq node.blackHeight leftHeight)

			(if (<> node.parent #void)
				(if (= node node.parent.left)
					(setq prev_step 1)
					(setq prev_step 2)))

			(setq node node.parent)
		)
		);cond
	);while
	true); end _check_redblack_tree

;; Node nodeX has just been inserted into table (by code in treeInsert).
;; It is possible that nodeX is a red node with a red parent, which is a violation
;; of the red-black tree properties. This function performs rotations and 
;; color changes to rebalance the tree
(defun _do_insert_balancing(table nodeX)
	;; In the first iteration of this loop, nodeX points to the red node just
	;; inserted in the tree. If the parent of nodeX exists (nodeX is not the root
	;; node) and is red, then the properties of the red-black tree are
	;; violated.
	;;
	;; At the start of any subsequent iterations, nodeX points to a red node
	;; with a red parent. In all other respects the tree is a legal red-black
	;; binary tree.

	vars:(nodeU nodeG)

	(while (and (<> nodeX table.head) (= nodeX.parent.isBlack 0))
	    ;; nodeG (grandfather) of nodeX must exist and must be black
		(setq nodeG nodeX.parent.parent)
		(if (or (= nodeG #void) (= nodeG.isBlack 0))
			(debug traceon:))
	
	    ;; nodeU (uncle) of pX may or may not exist.
		(if (= nodeX.parent nodeG.left)
			(setq nodeU nodeG.right)
			(setq nodeU nodeG.left))
	
			;; If the uncle of nodeX exists and is red, we do the following:
			;;       |                 |
			;;      G(b)              G(r)
			;;      /  \              /  \        
			;;   U(r)   P(r)       U(b)  P(b)
			;;            \                \
			;;           X(r)              X(r)
			;;
			;;     BEFORE             AFTER
			;; nodeX is then set to nodeG. If the parent of nodeG is red, then the while loop
			;; will run again.  
		(if (and (<> nodeU #void) (= nodeU.isBlack 0))
			(begin
			(setq nodeG.isBlack 0)
			(setq nodeU.isBlack 1)
			(setq nodeX.parent.isBlack 1)
			(setq nodeX nodeG)
			)
		else 
			(begin
			(if (= nodeX.parent nodeG.left)
				(begin
				(if (= nodeX nodeX.parent.right)
					(begin
					;; If nodeX is a right-child, do the following transform, essentially
					;; to change nodeX into a left-child: 
					;;       |                  | 
					;;      G(b)               G(b)
					;;      /  \               /  \        
					;;   P(r)   U(b)        X(r)  U(b)
					;;      \                /
					;;     X(r)            P(r) <-- new X
					;;
					;;     BEFORE             AFTER
					;;
					(setq nodeX nodeX.parent)
					(_leftRotate table nodeX)
	        		))
	
					;; Do the following transform, which balances the tree 
					;;      |                  | 
					;;     G(b)               P(b)
					;;     /  \               /  \        
					;;  P(r)   U(b)        X(r)  G(r)
					;;   /                         \
					;; X(r)                        U(b)
					;;
					;;    BEFORE             AFTER
					(if (<> nodeG nodeX.parent.parent)
						(debug traceon:))
					(setq nodeG.isBlack 0)
					(setq nodeX.parent.isBlack 1)
					(_rightRotate table nodeG)
				)
			else (begin
				;; This code is symetric to the illustrated case above.
				(if (= nodeX nodeX.parent.left)
					(begin
					(setq nodeX nodeX.parent)
					(_rightRotate table nodeX)
					))
				
				(if (<> nodeG nodeX.parent.parent)
					(debug traceon:))
				(setq nodeG.isBlack 0)
				(setq nodeX.parent.isBlack 1)
				(_leftRotate table nodeG)
				))))
    );while
	(setq table.head.isBlack 1)
	true);end _do_insert_balancing


;; A child of nodeP (Parent), which in turn had child nodexX, has just been removed from 
;; table (the figure below depicts the operation, Z is being removed). nodeP or nodexX, 
;; or both may be NULL.  
;;                |           |
;;                P           P
;;               / \         / \
;;              Z           X
;;             / \
;;            X  nil
;;
;; This function is only called if Z was black. In this case the red-black tree
;; properties have been violated, and nodeX has an "extra black". This function 
;; performs rotations and color-changes to re-balance the tree.
(defun _do_delete_balancing(table nodeX nodeP)
	vars:(nodeS) 	

	(while (and (<> nodeX table.head) (or ( = nodeX #void) (= nodeX.isBlack 1)))
		(if (= nodeX nodeP.left)
			(begin 
			(setq nodeS nodeP.right)
			(if (and (<> nodeS #void) (= nodeS.isBlack 0))
				(begin
				(setq nodeS.isBlack 1)
				(setq nodeP.isBlack 0)
				(_leftRotate table nodeP)
				(setq nodeS nodeP.right)
				))

			(if (= nodeS  #void)
				(setq nodeX nodeP)
			else (if (and 
					(or (= nodeS.left #void) (= nodeS.left.isBlack 1))
					(or (= nodeS.right #void) (= nodeS.right.isBlack 1)))
					(begin 
					(setq nodeS.isBlack 0)
					(setq nodeX nodeP)
					)
				 else
					(begin
					(if (or (= nodeS.right #void) (= nodeS.right.isBlack 1))
						(begin 
						(if (<> nodeS.left #void) (setq nodeS.left.isBlack 1))
						(setq nodeS.isBlack 0)
						(_rightRotate table nodeS)
						(setq nodeS nodeP.right)
						))
					(setq nodeS.isBlack nodeP.isBlack)
					(setq nodeP.isBlack 1)
					(if (<> nodeS.right #void) (setq nodeS.right.isBlack 1))
					(_leftRotate table nodeP)
					(setq nodeX table.head)
					)))
			)
		else
			(begin
			(setq nodeS nodeP.left)
			(if (and (<> nodeS #void) (= nodeS.isBlack 0) )
				(begin
				(setq nodeS.isBlack 1) 
				(setq nodeP.isBlack 0)
				(_rightRotate table nodeP)
				(setq nodeS nodeP.left)
				))
			(if (= nodeS #void )
				(setq nodeX nodeP)
			else 
				(if (and 
					(or (= nodeS.left #void) (= nodeS.left.isBlack 1))
					(or (= nodeS.right #void) (= nodeS.right.isBlack 1)))
					(begin 
					(setq nodeS.isBlack 0)
					(setq nodeX nodeP)
					)
				else
					(begin 
					(if(or (= nodeS.left #void) (= nodeS.left.isBlack 1))
						(begin
						(if (<> nodeS.right #void) (setq nodeS.right.isBlack 1))
						(setq nodeS.isBlack 0)
						(_leftRotate table nodeS)
						(setq nodeS nodeP.left)
						))
					(setq nodeS.isBlack nodeP.isBlack)
					(setq nodeP.isBlack 1)
					(if (<> nodeS.left #void) (setq nodeS.left.isBlack 1))
					(_rightRotate table nodeP)
					(setq nodeX table.head)
					)))
			))
		(setq nodeP nodeX.parent)
	);while
	(if (<> nodeX #void) (setq nodeX.isBlack 1))

	true); end _do_delete_balancing

;; createTable - create a new table in database
;; example: (setq iTable (btree.createTable mydb))
(defun createTable(mydb ...) 
	vars:(tableIdx table)

	(if (<> mydb.transState TRANS_NONE)
		(debug traceon:))

	(if (= (argCount) 2)
		(setq tableIdx (argFetch 1))
		(setq tableIdx mydb.next_idx))

	(setq mydb.next_idx (+ mydb.next_idx 1))
	(setq table (copy tableTemplate))
	(setq table.cursorsDir (new Directory:))
	(setq mydb.tablesDir[tableIdx] table)
	tableIdx); end of createTable


;; Log a single "rollback-op" for the given database.
(defun _logRollbackOp(mydb rollback)
	(cond
		((= mydb.transState TRANS_INTRANSACTION) 
			(setq rollback.next mydb.transRollback)
			(setq mydb.transRollback rollback)
		)
		((= mydb.transState TRANS_INCHECKPOINT)
			(if (= mydb.checkRollback #void)
				(setq mydb.checkRollbackTail rollback))
			(setq rollback.next mydb.checkRollback)
			(setq mydb.checkRollback rollback)
		)
		(true
			(debug traceon:)
		)
	); cond

	true); end _logRollbackOp

(defun dropTable(mydb tableIdx)
	vars:(rollback)
	(if (<> mydb.transState TRANS_NONE)
		(debug traceon:))

	(if (<> (length mydb.tablesDir[tableIdx].cursorsDir) 0)
		(debug traceon:))

	(if (>= tableIdx (length mydb.tablesDir))
		(return false)) ; dropTable failure!

	(clearTable mydb tableIdx)
	(^delete mydb.tablesDir[tableIdx])

	(if (<> mydb.transState TRANS_ROLLBACK)
		(begin
		(setq rollback (copy rollbackTemplate))
		(setq rollback.op ROLLBACK_CREATE)
		(setq rollback.tableIdx tableIdx)
		(_logRollbackOp mydb rollback)
		))
	true)


;; 
(defun keyCompare (cursor key nKey nIgnore) 
	(if (= cursor #void)
		(debug traceon:))

	(if (= cursor.node #void)
		(return -1)
	else 
		(begin
		(if (< (- cursor.node.nKey nIgnore) 0)
			(return -1)
			(return (_key_compare cursor.node.key (- cursor.node.nKey  nIgnore) key nKey)))))
	);end keyCompare


;; Get a new cursor for specified table of the supplied db. The wrFlag argument
;; indicates the cursor is open/closed for writing
(defun cursor(mydb tableIdx wrFlag)
	vars:(cursor)
	(setq cursor (copy cursorTemplate))
	(setq cursor.tableStruct mydb.tablesDir[tableIdx])
	(setq cursor.mydb mydb)
	(setq wrFlag wrFlag)
	(setq cursor.cursorsDir cursor.mydb.cursorsDir)
	(setq cursor.cursordDir[cursor] #void)
	cursor);end cursor

;; Insert a new record into the Rbtree.  The key is given by (pKey,nKey)
;; and the data is given by (pData,nData).  The cursor is used only to
;; define what database the record should be inserted into.  The cursor
;; is left pointing at the new record.
;;
;; If the key exists already in the tree, just replace the data. 
(defun insert (cursor key nKey dataInput nData)
	vars:(
		match
		node
		data
		)

	;; It is illegal to call insert() if we are
	;; not in a transaction 
	(if (<> cursor.mydb.transState TRANS_NONE)
		(debug traceon:))
	
	;; Make sure some other cursor isn't trying to read this same table 
	(if (= (_checkReadLocks cursor) 1)
		(return false)); The table pCur points to has a read lock */
	
	;; Grab the input data now, in case we need it for the  replace case 
	(setq data dataInput)
	
	;; Move the cursor to a node near the key to be inserted. If the key already
	;; exists in the table, then (match == 0). In this case we can just replace
	;; the data associated with the entry, we don't need to manipulate the tree.
	;; 
	;; If there is no exact match, then the cursor points at what would be either
	;; the predecessor (match == -1) or successor (match == 1) of the
	;; searched-for key, were it to be inserted. The new node becomes a child of
	;; this node.
	;; 
	;; The new node is initially red.
	(setq match (moveto cursor key nKey ))
	(if (<> match 0)
		(begin
		(setq node (copy nodeTemplate))
		(setq node.nKey nKey)
		(setq node.key key)
		(setq node.nData nData)
		(setq node.data data)
		(if (<> cursor.node #void)
			(begin
			(cond 
			((= match -1)
				(if (= cursor.node.right #void)
					(debug traceon:))
				(setq node.parent cursor.node)
				(setq cursor.node.right node)
			)
			((= match 1)
				(if (= cursor.node.left #void)
					(debug traceon:))
				(setq node.parent cursor.node)
				(setq cursor.node.left node)
			)
			);end cond
			)
		else
			(setq cursor.tableStruct.head node))
	
		;; Point the cursor at the node just inserted
		(setq cursor.node node)
		
		;; A new node has just been inserted, so run the balancing code 
		(_do_insert_balancing cursor.tableStruct node)
		
		;; Set up a rollback-op in case we have to roll this operation back 
		(if (<> cursor.mydb.transState TRANS_ROLLBACK)
			(begin
			(setq rollback (copy rollbackTemplate))
			(setq rollback.op ROLLBACK_DELETE)
			(setq rollback.tableIdx cursor.tableIdx)
			(setq rollback.nKey cursor.node.nKey)
			(setq rollback.key cursor.node.key)
			(_logRollbackOp cursor.mydb rollback)
			))
		)
	else
		(begin
		;; No need to insert a new node in the tree, as the key already exists.
		;; Just clobber the current nodes data. 
	
		;; Set up a rollback-op in case we have to roll this operation back 
		(if (<> cursor.mydb.transState TRANS_ROLLBACK)
			(begin
			(setq rollback (copy rollbackTemplate))
			(setq rollback.tableIdx cursor.tableIdx)
			(setq rollback.nKey cursor.node.nKey)
			(setq rollback.key cursor.node.key)
			(setq rollback.nData cursor.node.nData)
			(setq rollback.data cursor.node.data)
			(setq rollback.op ROLLBACK_INSERT)
			))
	
		;; Actually clobber the nodes data
		(setq cursor.node.data data)
		(setq cursor.node.nData nData)
		))
	
	true);end insert



;; Move the cursor so that it points to an entry near pKey.
;;     Res<0      The cursor is left pointing at an entry that
;;                  is smaller than pKey or if the table is empty
;;                  and the cursor is therefore left point to nothing.
;;
;;     Res==0     The cursor is left pointing at an entry that
;;                  exactly matches pKey.
;;
;;     Res>0      The cursor is left pointing at an entry that
;;                  is larger than pKey.
;		Res==Error	
(defun moveto(cursor key, nKey)
	vars:(tempNode Res)
	(setq tempNode #void)

	(setq cursor.node cursor.mydb.head)
	(setq Res -1)
	(while (and (<> cursor.node #void) (<> Res 0))
		(setq Res (_key_compare cursor.node.key cursor.node.nKey key nKey))
		(if (= Res false)
			(return Res)); moveto failed!

		(setq tempNode cursor.node)
		(cond 
			((= Res 1) ;; cursor > key
				(setq cursor.node cursor.node.left)
			)
			((= Res -1) ;; cursor < key
				(setq cursor.node cursor.node.right)
			)
		)
	);end while

	;; If (= cursor.node #void), then we have failed to find a match. Set
	;; cursor.node to tempNode, which is either #void (if the tree is empty) or the
	;; last node traversed in the search. In either case the relationship
	;; between tempNode and the searched for key is already stored in Res. tempNode is
	;; either the successor or predecessor of the key we tried to move to.
	(if (= cursor.node #void)
		(setq cursor.node tempNode))

	(setq cursor.skip SKIP_NONE)

	Res)


;; Delete the entry that the cursor is pointing to.
;;
;; The cursor is left pointing at either the next or the previous
;; entry.  If the cursor is left pointing to the next entry, then 
;; the cursor.skip flag is set to SKIP_NEXT which forces the next call to 
;; next() to be a no-op.  That way, you can always call
;; next() after a delete and the cursor will be left
;; pointing to the first entry after the deleted entry.  Similarly,
;; cursorskip is set to SKIP_PREV is the cursor is left pointing to
;; the entry prior to the deleted entry so that a subsequent call to
;; previous() will always leave the cursor pointing at the
;; entry immediately before the one that was deleted.

(defun delete (cursor)
	vars:(
		nodeZ 			;; The one being deleted
		nodeChild		;; The child of the spliced out node
		rollback		;; rollback record
		nodeTemp		;; temporary node
		dummy
		result
		)

	; It is illegal to call sqliteRbtreeDelete() if we are
	;; not in a transaction
	(if (= cursor.mydb.transState TRANS_NONE)
		(debug traceon:))

	;; Make sure some other cursor isn't trying to read this same table
	(if (= (_checkReadLocks cursor) 1)
		(return false)) ;; delete failed!

	(setq nodeZ cursor.node)
	(if (= nodeZ #void)
		(return true))

	;; If we are not currently doing a rollback, set up a rollback op for this 
	;; deletion
	(if (<> cursor.mydb.transState TRANS_ROLLBACK)
		(begin
		(setq rollback (copy rollbackTemplate))
		(setq rollback.tableIdx cursor.tableIdx)
		(setq rollback.nKey nodeZ.nKey)
		(setq rollback.key nodeZ.key)
		(setq rollback.nData nodeZ.nData)
		(setq rollback.data nodeZ.data)
		(setq rollback ROLLBACK_INSERT)
		(_logRollbackOp cursor.mydb rollback)
		))

	;; First do a standard binary-tree delete (nodeZ is to be deleted). How
	;; to do this depends on how many children nodeZ has:
	;;
	;; If nodeZ has no children or one child, then splice out nodeZ.  If nodeZ has two
	;; children, splice out the successor of nodeZ and replace the key and data of
	;; nodeZ with the key and data of the spliced out successor.  */
	(if (and (<> nodeZ.left #void) (<> nodeZ.right #void))
		(begin
		(setq nodeTemp (copy nodeTemplate))
		(setq cursor.skip SKIP_NONE)
		(setq result (next cursor))
		(if (<> result  0)
			(debug traceon:))
		(setq nodeZ.data cursor.node.data)
		(setq nodeZ.nData cursor.node.nData)
		(setq nodeZ.key cursor.node.key)
		(setq nodeZ.nKey cursor.node.nKey)
		(setq nodeTemp nodeZ)
		(setq nodeZ cursor.node)
		(setq cursor.node nodeTemp)
		(setq cursor.skip SKIP_NEXT)
		 ) 
	else 
		(begin
		(setq cursor.skip SKIP_NONE)
		(setq result (next cursor))
		(setq cursor.skip SKIP_NEXT)
		(if (<> result 0)
			(begin
			(setq result (last cursor))
			(setq result (previous cursor))
			(setq cursor.skip SKIP_NEXT)
			))
		))

	;; pZ now points at the node to be spliced out.
	 (if (or (= nodeZ.left #void) (= nodeZ.right #void))
		(debug traceon:))

	(if (= nodeZ.left #void)
		(setq nodeChild nodeZ.right)
		(setq nodeChild nodeZ.left))
	(if  (<> nodeZ.parent #void)
		(begin
		(if (or (= nodeZ nodeZ.parent.left) (= nodeZ nodeZ.parent.right))
			(debug traceon:))
		(if (= nodeZ nodeZ.parent.left)
			(setq nodeZ.parent.left nodeChild)
			(setq nodeZ.parent.right nodeChild))
		)
	else
		(begin
		(setq cursor.tableStruct.head nodeChild)
		))
	(if (<> nodeChild #void)
		(setq nodeChild.parent nodeZ.parent))


	;; nodeZ now points at the spliced out node. nodeChild is the only child of nodeZ, or
	;; NULL if nodeZ has no children. If nodeZ is black, and not the tree root, then we
	;; will have violated the "same number of black nodes in every path to a
	;; leaf" property of the red-black tree. The code in _do_delete_balancing()
	;; repairs this.
	(if (= nodeZ.isBlack 1)
		(_do_delete_balancing cursor.tableStruct nodeChild nodeZ.parent))

	true); end delete


(defun clearTable(mydb tableIdx) 
	vars:(table node tempNode rollback)

	(if (not (isMember tableIdx mydb.tablesDir[tableIdx]))
		(debug traceon:))

	(setq table mydb.tablesDir[tableIdx])
	
	(setq node table.head)
	(while (<> node #void)
		(if (<> node.left #void)
			(setq node node.left)
		else
		(if (<> node.right #void)
			(setq node node.right)
		else
			(begin
			(setq tempNode node.parent)
			(if (= mydb.transState TRANS_ROLLBACK)
				(begin
				; No Operation
				)
			else 
				(begin
				(setq rollback (copy rollbackTemplate))
				(setq rollback.op ROLLBACK_INSERT)
				(setq rollback.tableIdx tableIdx)
				(setq rollback.nKey node.nKey)
				(setq rollback.key node.key)
				(setq rollback.nData node.nData)
				(setq rollback.data node.data)
				(_logRollbackOp mydb rollback)
				))
			(if (<> tempNode #void)
				(if (= tempNode.left node)
					(setq tempNode.left #void)
					(if (= tempNode.right node)
						(setq tempNode.right #void))))
			(setq node tempNode)
			)))
	);while

	(setq table.head #void)
	true);end clearTable

;; Find first node in cursor
;; Returns 0 if no node, 1 if node found
(defun first (cursor)
	(if (<> cursor.tableStruct.head #void)
		(begin
		(setq cursor.node cursor.tableStruct.head)
		(while (<> cursor.node.left #void)
			(setq cursor.node cursor.node.left))))
	
	(setq cursor.skip SKIP_NONE)

	(if (<> cursor.node #void)
		(return 0)
		(return 1))
	); end first

;; Find last node in cursor
;; Returns 0 if no node, 1 if node found
(defun last (cursor)
	(if (<> cursor.tableStruct.head #void)
		(begin
		(setq cursor.node cursor.tableStruct.head)
		(while (<> cursor.node.right #void)
			(setq cursor.node cursor.node.right))))

	(setq cursor.skip SKIP_NONE)

	(if (<> cursor.node #void)
		(return 0)
		(return 1))
	);end last

;; Advance the cursor to the next entry in the database.  If
;; successful then return 0.  If the cursor
;; was already pointing to the last entry in the database before
;; this routine was called, then return 1.
(defun next	(cursor)
	vars:(nodeX)
	(if (and (<> cursor.node #void) (<> cursor.skip SKIP_NEXT))
		(if (<> cursor.node.rigth #void)
			(begin
			(setq cursor.node cursor.node.right)
			(while (<> cursor.node.left #void)
				(setq cursor.node cursor.node.left)))
		else
			(begin
			(setq nodeX cursor.node)
			(setq cursor.node nodeX.parent)
			(while (and (<> cursor.node #void) (= cursor.node.right nodeX))
				(setq nodeX cursor.node)
				(setq cursor.node nodeX.parent)))))

	(setq cursor.skip SKIP_NONE)

	(if (= cursor.node #void)
		(return 1)
		(return 0))

	);end next


(defun previous	(cursor)
	vars:(nodeX)
	(if (and (<> cursor.node #void) (<> cursor.skip SKIP_PREV))
		(if (<> cursor.node.left #void)
			(begin
			(setq cursor.node cursor.node.left)
			(while (<> cursor.node.right #void)
				(setq cursor.node cursor.node.right))))
	else
		(begin
		(setq nodeX cursor.node)
		(setq cursor.node nodeX.parent)
		(while (and (<> cursor.node #void) (= cursor.node.left nodeX))
			(setq nodeX cursor.node)
			(setq cursor.node nodeX.parent))))

	(setq cursor.skip SKIP_NONE)
	(if (= cursor.node #void)
		(return 1)
		(return 0))
	);end previous

(defun keySize (cursor)
	(if (<> cursor.node #void)
		(return cursor.node.nKey)
		(return 0))
	);end keySize

; Return all or some of the cursors current node's key
(defun key (cursor offset amt) 
	(if (= cursor.node #void)
		(return #void))
	(if (or (= cursor.node.key #void) (<= (+ amt offset) cursor.node.nKey))
		(return (mid cursor.node.key offset amt))
		(return (substring cursor.node.key offset)))
	#void)

(defun dataSize	(cursor)
	(if (= cursor.node #void)
		(return 0)
		(return cursor.node.nData))
	);end dataSize


(defun data	(cursor offset amt) 
	(if (= cursor.node.data #void)
		(return #void))

	(if (<= (+ amt offset) cursor.node.nData)
		(return (mid cursor.node.data offset amt))
		(return (substring cursor.node.data offset)))
	#void)


;; Remove cursor from cursorsDir directory in table
(defun closeCursor (cursor) 
	vars:(n N)
	(if (isMember cursor cursor.tableStruct.cursorsDir)
		(^delete cursor cursor.tableStruct.cursorsDir))
	true)	

(defun getMeta(mydb aMetaIdx)
	(return (copy mydb.metaData)))

(defun updateMeta(mydb metaData)
		(setq mydb.metaData (copy metaData)))

;; Check that each table in mydb meets the requirements for a red/black
;; binary tree. If an error is found, writeln an explanation of the problem.
;; Arguments aRoot and nRoot are ignored for this in-memory database.
(defun integrityCheck(mydb aRoot nRoot)
	vars:(t T)
	(setq T (length mydb.tablesDir))
	(loop for t from 0 until T do 
		(_check_redblack_tree mydb.tablesDir[t]))
	true);end integrityCheck


;; setCacheSize is a noop in the memory database
(defun setCacheSize (mydb sz) true)

;; setSafetyLevel is a noop in the memory database
(defun setSafetyLevel(mydb sl) true)

(defun beginTrans(mydb) 
	(if (<> mydb.transState TRANS_NONE)
		(return false))
	(if (<> mydb.transRollback 0)
		(debug traceon:))
	(setq mydb.transState TRANS_INTRANSACTION)
	true); end beginTrans


(defun _deleteRollbackList(rollback)
	(debug traceon:) ; this function is obsolete. Just #void the assignment 
					 ; and the list will be deleted in the next gc
true)

(defun commit(mydb) 
  	;; Just delete pTransRollback and pCheckRollback */
	(setq mydb.transRollback #void)
	(setq mydb.checkRollback #void)
	(setq mydb.checkRollbackTail #void)
	(setq mydb.transState TRANS_NONE)
	true);end of commit


;; close - close 
;; example: (setq mydb (btree.close mydb))
(defun close() #void)

;; Execute and delete the supplied rollback-list on pRbtree.
;; rollbackList must be one of:
;;	transRollback:, checkRollback:, checkRollbackTail:
(defun _execute_rollback_list(mydb rollbackList)
	vars:(temp cur rollback result)

	(cond
		((= rollbackList transRollback:) (setq rollback mydb.transRollback))
		((= rollbackList checkRollback:) (setq rollback mydb.checkRollback))
		((= rollbackList checkRollbackTail:) (setq rollback mydb.checkRollbackTail))
		(true
			(debug traceon:))
	)

	(setq cur (copy cursorTemplate))
	(setq cur.mydb mydb)
	(setq cur.wrFlag 1)

 	(while (<> rollback #void )
		(cond
		((= rollback.op ROLLBACK_INSERT)
			(setq cur.tableStruct mydb.tables[rollback.tableIdx])
			(if (= cur.tableStruct #void)
				(debug traceon:))
			(setq cur.tableIdx rollback.tableIdx)
			(setq cur.skip SKIP_NONE)
			(btree.insert cur rollback.key rollback.nKey rollback.data rollback.nData)
		)

		((= rollback.op ROLLBACK_DELETE)
			(setq cur.tableStruct mydb.tables[rollback.tableIdx])
			(if (= cur.tableStruct #void)
				(debug traceon:))
			(setq cur.tableIdx rollback.tableIdx)
			(setq cur.skip SKIP_NONE)
			(setq result (moveto cur rollback.key rollback.nKey))
			(if (<> result true)
				(debug traceon:))
			(delete cur)
		)

		((= rollback.op ROLLBACK_CREATE)
			(createTable mydb rollback.tableIdx))

		((= rollback.op ROLLBACK_DROP)
			(dropTable mydb rollback.tableIdx))
    	)

	(setq rollback rollback.next); Note the old rollback  will be garbage collected
	
  	);end while

	true);end _execute_rollback_list

(defun rollback(mydb)
	(setq mydb.transState TRANS_ROLLBACK)
	(_execute_rollback_list mydb checkRollback:)
	(_execute_rollback_list mydb transRollback:)
	(setq mydb.transRollback #void)
	(setq mydb.checkRollback #void)
	(setq mydb.checkRollbackTrail #void)
	(setq mydb.transState TRANS_NONE)
	true);end rollback

(defun beginCkpt(mydb) 
	(if (<> mydb.transState TRANS_INTRANSACTION)
		(return false))
	(if (<> mydb.checkRollback #void)
		(debug traceon:))
	(if (<> mydb.checkRollbackTail #void)
		(debug traceon:))
	(setq mydb.transState TRANS_INCHECKPOINT)
	true); end beginCkpt


(defun commitCkpt(mydb) 
	(if (= mydb.transState TRANS_INCHECKPOINT)
		(begin
		(if (<> mydb.checkRollBack #void)
			(begin
			(setq mydb.checkRollbackTail.next mydb.transRollback)
			(setq mydb.transRollback mydb.checkRollback)
			(setq mydb.checkRollback #void)
			(setq mydb.checkRollbackTail #void)
			))
		(setq mydb.transState TRANS_INTRANSACTION)
		))
	true);end commitCkpt

(defun rollbackCkpt(mydb) 
	(if (<> mydb.transState TRANS_INCHECKPOINT)
		(return true))
	(setq mydb.transState TRANS_ROLLBACK)
	(_execute_rollback_list mydb mydb.checkRollback)
	(setq mydb.checkRollback #void)
	(setq mydb.checkRollbackTail #void)
	(setq mydb.transState TRANS_INTRANSACTION)
	true);end rollbackCkpt


;; createIndex - create a new index in database
;; example: (setq iIndex (btree.createIndex mydb))
;; NOTE: At this level and for an in-memory database a table and an index have the same
;; implementation.
(defun createIndex(mydb) 
	vars:(tableIdx table)
	(setq tableIdx mydb.next_idx)
	(setq mydb.next_idx (+ mydb.next_idx 1))
	(setq table (copy tableTemplate))
	(setq table.cursorsDir (new Directory:))
	(setq mydb.tablesDir[tableIdx] table)
	tableIdx); end of createIndex

;; selfTest - performs an exhaustive test of the btree Lambda
;; example: (btree.selfTest)
(deforphan btree:selfTest() 




true)

true)
