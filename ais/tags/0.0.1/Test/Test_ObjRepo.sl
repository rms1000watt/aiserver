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
;;  Title:    Object Repository Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the Object Repository
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_ObjRepo.sl")

;;************************************************************************
;; Test script global variables
;;************************************************************************
(setq scriptName "Object Repository Test")
(define _DBPathName "Test.db")
(define _HCPathName "Honeycomb.db")
(define n 10)
(define m 300)

;;************************************************************************
;;  Test Script diagnostic and utility functions
;;************************************************************************
(defun diagnostic(msg) (error "objrepository" msg))

(defun testit(evalTxt result)
 ;(writeln evalTxt) 
 (setq lt evalTxt)
 (setq lr result)
 (if (not (equal (eval evalTxt) result ))
     (begin
        (writeln scriptName " *FAILURE* " evalTxt)
        (error scriptName)
        ) 
   else
     true
   ) ; end if
 (gc))

(define (readTextFile name) 
   vars:(fileID self (type 0))
   (setq fileID (fileOpen name 0 type))
   (setq self (fileRead fileID))
   (fileClose fileID 1)
   self)

;; *******************************************************************
;; summary:  Perform a read of a record in the specified honeycomb file.
;; Parms:    This procedure accepts two arguments.
;;           fileID:    The fileID of the honeycomb file to read.
;;           recIndex:  The record index of the record to be read.
;; Return:   The object record contents.
;; *******************************************************************
(defun honeyReadRecord(fileID recIndex) 
   vars:(record (recordSize 2000))
   (fileSeek fileID (muli recIndex recordSize) 1)
   (setq record (loadObject fileID))
   ; Took longer with compression
   ;(setq record (uncompress record))
   ;(setq record (loadObject record))
   record) ;; end honeyReadRecord

(defun honeyWriteRecord(fileID recIndex record) 
;; *******************************************************************
;; summary:  Perform a write of a record in the specified honeycomb file.
;; Parms:    This procedure accepts three arguments.
;;           fileID:    The fileID of the honeycomb file to read.
;;           recIndex:  The record index of the record to be read.
;;           record:    The record object to be written.
;; Return:   The object record contents.
;; *******************************************************************
   vars:((recordSize 2000))
   (fileSeek fileID (muli recIndex recordSize) 1)
   ; Took longer with compression
   ;(setq record (saveObject record))
   ;(setq record (compress record))
   (saveObject fileID record)
   true) ;; end honeyWriteRecord

(defun honeyWriteBlockOld(fileID blkIndex record) 
;; *******************************************************************
;; summary:  Perform a read of a block in the specified honeycomb file.
;; Parms:    This procedure accepts two arguments.
;;           fileID:    The fileID of the honeycomb file to read.
;;           blkIndex:  The block index of the block to be read.
;; Return:   The object record contents.
;; *******************************************************************
   vars:(i block (recordSize 2000) (blockSize 300))
   (fileSeek fileID (muli blkIndex recordSize blockSize) 1)
   (loop for i from 0 until blockSize do
      (setq block (saveObject record))   
      ) ;; end loop
   (setq block (new Vector: byte: (muli recordSize blockSize)))
   (fileWrite fileID block)
   true) ;; end honeyWriteBlock

(defun honeyWriteBlock(fileID blkIndex record) 
;; *******************************************************************
;; summary:  Perform a read of a block in the specified honeycomb file.
;; Parms:    This procedure accepts two arguments.
;;           fileID:    The fileID of the honeycomb file to read.
;;           blkIndex:  The block index of the block to be read.
;; Return:   The object record contents.
;; *******************************************************************
   vars:(i block (recordSize 2000) (blockSize 300))
   (loop for i from 0 until blockSize do
      (fileSeek fileID (muli i recordSize) 1)
      (saveObject fileID record) ;;TM? How do we know the record will fit?
      ) ;; end loop
   (fileSeek fileID (muli (addi blkIndex 1) recordSize blockSize) 1)
   (fdisplay fileID _eof)
   true) ;; end honeyWriteBlock

(defun honeyReadBlock(fileID blkIndex record) 
;; *******************************************************************
;; summary:  Perform a read of a block in the specified honeycomb file.
;; Parms:    This procedure accepts two arguments.
;;           fileID:    The fileID of the honeycomb file to read.
;;           blkIndex:  The block index of the block to be read.
;; Return:   The object record contents.
;; *******************************************************************
   vars:(i block (recordSize 2000) (blockSize 300))
   (fileSeek fileID (muli blkIndex recordSize blockSize) 1)
   (setq block (fileRead fileID (muli recordSize blockSize)))
   (loop for i from 0 until blockSize do
;;      (setq block (loadObject record)) ;;TM? This is strange -- Bug?
  		(setq record (loadObject block)) 
     ) ;; end loop
   record) ;; end honeyReadBlock

;;************************************************************************
;;  Start the Test Script
;;************************************************************************
(writeln   scriptName " started")

;;************************************************************************
;;  Create the test Repository.
;;************************************************************************
(define db (new ObjectRepository: _DBPathName clear:))

;;************************************************************************
;;  Write n records of fixed length to the Repository.
;;  Note: No buffering, and no transaction management.
;;************************************************************************
(writeln "1. Write n Lambdas to the Repository.") 
(writeln "Note: No buffering, and no transaction management.") 
(loop for i from 0 until n do
   (setq dbrecord (new honeyWriteBlock)) ;;Any record will do
   (setf db[i] dbrecord)
   (testit "db[i]" dbrecord)
   (setq db[i] dbrecord)
   (testit "db[i]" dbrecord)
   )
(testit "(length db)" n)
(clear db)

;;************************************************************************
;;  Write n records of random length to the Repository.
;;  Note: No buffering, and no transaction management.
;;************************************************************************
(writeln "2. Write n records of random length to the Repository.") 
(writeln "Note: No buffering, and no transaction management.") 
(define db (new ObjectRepository: _DBPathName)) ;;TM? - should we be do a clear: ?
(loop for i from 0 until n do
   (setq dbrecord (new Vector: (addi (random n) 1) i))
   (setf db[i] dbrecord)
   (testit "db[i]" dbrecord)
   (setq db[i] dbrecord)
   (testit "db[i]" dbrecord)
   )
(testit "(length db)" n)

;;************************************************************************
;;  Renaming a record saved in the Repository.
;;************************************************************************
(writeln "3. Renaming a record saved in the Repository.")
(setq dbrecord db[1])
(rename db 1 -1) 
(testit "db[1]" #void)
(testit "db[-1]" dbrecord)

;;************************************************************************
;;  Create the test Repository.
;;************************************************************************
(define db (new ObjectRepository: _DBPathName clear:))

;;************************************************************************
;;  Write n records of random length to the Repository.
;;  Note: No buffering, and no transaction management.
;;************************************************************************
(writeln "4. Write n records of random length to the Repository.") 
(writeln "Note: No buffering, and no transaction management.") 
(define db (new ObjectRepository: _DBPathName))
(loop for i from 0 until n do
   (setq dbrecord (new Vector: (addi (random n) 1) i))
   (setf db[i] dbrecord)
   (testit "db[i]" dbrecord)
   (setq db[i] dbrecord)
   (testit "db[i]" dbrecord)
   )
(testit "(length db)" n)

;;************************************************************************
;;  Update n records of random length to the Repository.
;;  Note: Yes buffering, and no transaction management.
;;************************************************************************
(writeln "5. Write n records of random length to the Repository.") 
(writeln "Note: Yes buffering, and no transaction management.") 
(define db (new ObjectRepository: _DBPathName buffer: 4))
(loop for i from 0 until n do
   (setq dbrecord (new Vector: (addi (random n) 1) i))
   (setq db[i] dbrecord)
   (testit "db[i]" dbrecord)
   )
(testit "(length db)" n)

;;************************************************************************
;;  Update n records of random length to the Repository.
;;  Note: Yes buffering, and Yes transaction management.
;;************************************************************************
(writeln "6. Write n records of random length to the Repository.") 
(writeln "Note: Yes buffering, and Yes transaction management.") 
(define db (new ObjectRepository: _DBPathName buffer: 4))
(beginTransaction db)
(loop for i from 0 until n do
   (setq dbrecord (new Vector: (addi (random n) 1) i))
   (setq db[i] dbrecord)
   (testit "db[i]" dbrecord)
   )
(commitTransaction db)
(testit "(length db)" n)

;;************************************************************************
;;  Erase n records of random length to the Repository.
;;  Note: Yes buffering, and Yes transaction management.
;;************************************************************************
(writeln "7. Erase n records of random length to the Repository.") 
(writeln "Note: Yes buffering, and Yes transaction management.") 
(define db (new ObjectRepository: _DBPathName buffer: 4))
(beginTransaction db)
(loop for i from 0 until n do
   (setq db[i] #void)
   (testit "db[i]" #void)
   )
(abortTransaction db)
(testit "(length db)" n)

;;************************************************************************
;;  Erase n records of random length to the Repository.
;;  Note: no buffering, and no transaction management.
;;************************************************************************
(writeln "8. Erase n records of random length to the Repository.") 
(writeln "Note: Yes buffering, and Yes transaction management.") 
(define db (new ObjectRepository: _DBPathName))
(loop for i from 0 until n do
   (setq db[i] #void)
   (testit "db[i]" #void)
   )
(testit "(length db)" 0)

;;************************************************************************
;;  Write n records with complex keys to the Repository.
;;  Note: Yes buffering, and Yes transaction management.
;;************************************************************************
(writeln "9. Write n records with complex keys to the Repository.") 
(writeln "Note: Yes buffering, and Yes transaction management.") 
(define db (new ObjectRepository: _DBPathName buffer: 4))
(beginTransaction db)
(loop for i from 0 until n do
   (setq dbkey (new Vector: i i))
   (setq dbrecord (new Vector: (addi (random n) 1) i))
   (setq db[dbkey] dbrecord)
   (testit "db[dbkey]" dbrecord)
   )
(commitTransaction db)
(testit "(length db)" n)

;;************************************************************************
;;  Update n records with complex keys to the Repository.
;;  Note: Yes buffering, and Yes transaction management.
;;************************************************************************
(writeln "10. Update n records with complex keys to the Repository.") 
(writeln "Note: Yes buffering, and Yes transaction management.") 
(define db (new ObjectRepository: _DBPathName buffer: 4))
(beginTransaction db)
(loop for i from 0 until n do
   (setq dbkey (new Vector: i i))
   (setq dbrecord (new Vector: (addi (random n) 1) i))
   (setq db[dbkey] dbrecord)
   (testit "db[dbkey]" dbrecord)
   )
(commitTransaction db)
(testit "(length db)" n)

;;************************************************************************
;;  Read n records sequential with complex keys from the Repository.
;;  Note: Yes buffering, and Yes transaction management.
;;************************************************************************
(writeln "11. Read n records sequential with complex keys from the Repository.") 
(writeln "Note: Yes buffering, and Yes transaction management.") 
(define db (new ObjectRepository: _DBPathName buffer: 4))
(beginTransaction db)
(loop for i from 0 until n do
   (setq dbkey (new Vector: i i))
   (setq dbrecord (new Vector: (addi (random n) 1) i))
   (setq db[dbkey] dbrecord)
   (testit "db[i 0]" dbkey)
   (testit "db[i 1]" dbrecord)
   (testit "db[position: dbkey]" i)
   )
(commitTransaction db)
(testit "(length db)" n)

;;************************************************************************
;;  Erase n records with complex keys from the Repository.
;;  Note: Yes buffering, and Yes transaction management.
;;************************************************************************
(writeln "12. Erase n records with complex keys from the Repository.") 
(writeln "Note: Yes buffering, and Yes transaction management.") 
(define db (new ObjectRepository: _DBPathName buffer: 4))
(beginTransaction db)
(loop for i from 0 until n do
   (setq dbkey (new Vector: i i))
   (setq db[dbkey] #void)
   (testit "db[dbkey]" #void)
   )
(abortTransaction db)
(testit "(length db)" n)

;;************************************************************************
;;  Erase n records with complex keys from the Repository.
;;  Note: no buffering, and no transaction management.
;;************************************************************************
(writeln "13. Erase n records with complex keys from the Repository.") 
(writeln "Note: Yes buffering, and Yes transaction management.") 
(define db (new ObjectRepository: _DBPathName clear:))
(loop for i from 0 until n do
   (setq dbkey (new Vector: i i))
   (setq db[dbkey] #void)
   (testit "db[dbkey]" #void)
   )
(testit "(length db)" 0)

;;************************************************************************
;;  Write n records of random length to the Repository.
;;  Note: No buffering, and no transaction management.
;;************************************************************************
(writeln "14. Write n records of random length to the Repository.") 
(writeln "Note: No buffering, and no transaction management.") 
(clear db)
(loop for i from 0 until n do
   (setq dbrecord (new Vector: (addi (random n) 1) i))
   (setq db[i] dbrecord)
   (testit "db[i]" dbrecord)
   )
(testit "(length db)" n)
                                                                     S
;;************************************************************************
;;  Recreate the test Repository for use in blocking tests.
;;************************************************************************
(define db (new ObjectRepository: _DBPathName clear:))

;;************************************************************************
;;  Perform general tests of child repositories and inspection of the Repository.
;;  Note: No buffering, and no transaction management.
;;************************************************************************
(writeln "15. Perform general tests of child repositories and inspection of the Repository.") 
(writeln "Note: No buffering, and no transaction management.")
  
(systemCheck)
(define db (new ObjectRepository: _DBPathName clear:)) 
(setq db.x 22)
(if (<> db.x 22) (error "repo" "Repository Test Failed on db.x"))
(saveRepository db y: 30000)
db.y
(setq db.y.x 33)

(if (<> db.y.x 33) (error "repo" "Repository Test Failed on db.y.x"))
(saveRepository db y: #{dir| a: #(1 2 3) b: #(4 5 6)})
(testit "db.y.a" #(1 2 3))
(testit "db.y.b" #(4 5 6))
(testit "(length db.y)" 2)

(setq nd (new Directory:))
(loop for i from 0 until m do
   (setq nd[i] (new Vector: 3 1 2 3))
   ) ;; end loop
(saveRepository db y: nd)
(if (<> db.y[0][0] 1) (error "repo" "Repository Test Failed on db.y.x"))
(if (<> db.y[2][1] 2) (error "repo" "Repository Test Failed on db.y.y[1]"))
(setq nx (loadRepository db.y))
(if (<> nx[0] nd[0]) (error "repo" "Repository Test Failed on nx.x"))
(if (<> nx[1] nd[1]) (error "repo" "Repository Test Failed on nx.y"))
(inspect db show:)
(inspect db.y show:)
(writeln "Length of db.y = " (inspect db length: y:))
(writeln "Date stamp for db.y = " (inspect db date: y:))
(writeln "Length of db.y.y = " (inspect db.y length: y:))
(writeln "(writeln "Length of db.y.y = " (inspect db.y length: y:))")
(writeln "Date stamp for db.y.y = " (inspect db.y date: y:))

;;************************************************************************
;;  Prepare a block of n records of fixed length for Repository testing.
;;  Note: Clear the repository.
;;************************************************************************
(clear db)
(setq dbblock (new Directory:))
(setq mbblock (new Directory:))
(setq av (new Vector: object: 100))
(loop for i from 0 until m do
   (setq dbblock[i] (new Vector: 100 i))
   (setq av[i] (symbol (append "col" i)))
   ) ;; end loop

;;************************************************************************
;;  Block write as a single record.
;;  Note: Single block write, No transaction management.
;;************************************************************************
(writeln "16. Block write as a single record.") 
(writeln "Note: Single block write, No buffering, and no transaction management.") 
(gc)(setq startTime (getTickCount 0))
(loop for i from 0 until m do
   (setq mbblock[i] dbblock[i])
   ) ;; end loop
(setq db.b mbblock)
(setq endTime (getTickCount startTime))
(writeln "Timing test for block write as a single record = " endTime " seconds.")

;;************************************************************************
;;  Block read as a single record.
;;  Note: Single record read, No transaction management.
;;************************************************************************
(writeln "17. Block read as a single record.") 
(writeln "Note: Single block read, No buffering, and no transaction management.")
(abortTransaction db)
(gc)(setq startTime (getTickCount 0))
(setq x db.b)
(loop for i from 0 until m do
   (setq mbblock[i] x[i])
   ) ;; end loop
(setq endTime (getTickCount startTime))
(testit "mbblock" dbblock)
(writeln "Timing test for block read as a single record = " endTime " seconds.")

;;************************************************************************
;;  Block write as individual records.
;;  Note: Individual record writes, No transaction management.
;;************************************************************************
(writeln "18. Block write as individual records.") 
(writeln "Note: Individual record writes, No transaction management.") 
(clear db)
(setq startTime (getTickCount 0))
(loop for i from 0 until m do
   (setq db[i] dbblock[i])
   )
(setq endTime (getTickCount startTime))
(writeln "Timing test for block write as individual records = " endTime " seconds.")

;;************************************************************************
;;  Block write as individual records.
;;  Note: Individual record writes, Yes transaction management.
;;************************************************************************
(writeln "19. Block write as individual records.") 
(writeln "Note: Individual record writes, Yes transaction management.") 
(clear db)
(gc)(setq startTime (getTickCount 0))
(beginTransaction db)
(loop for i from 0 until m do
   (setq db[i] dbblock[i])
   )
(commitTransaction db)
(setq endTime (getTickCount startTime))
(writeln "Timing test for block write as individual records = " endTime " seconds.")

;;************************************************************************
;;  Block read as individual records.
;;  Note: Individual record reads, No transaction management.
;;************************************************************************
(writeln "20. Block read as individual records.") 
(writeln "Note: Individual record reads, No transaction management.")
(gc)(setq startTime (getTickCount 0))
(loop for i from 0 until m do
   (setq mbblock[i] db[i])
   ) ;; end loop
(setq endTime (getTickCount startTime))
(writeln "Timing test for block read as individual records = " endTime " seconds.")
(loop for i from 0 until m do
   (testit "mbblock[i]" dbblock[i])
   ) ;; end of block read loop

;;************************************************************************
;;  Block read as individual records.
;;  Note: Individual record reads, Yes transaction management.
;;************************************************************************
(writeln "21. Block read as individual records.") 
(writeln "Note: Individual record reads, Yes transaction management.")
(setq startTime (getTickCount 0))
(beginTransaction db)
(loop for i from 0 until m do
   (setq mbblock[i] db[i])
   ) ;; end loop
(abortTransaction db)
(setq endTime (getTickCount startTime))
(writeln "22. Timing test for block read as individual records = " endTime " seconds.")
(loop for i from 0 until m do
   (testit "mbblock[i]" dbblock[i])
   ) ;; end of block read loop

;;************************************************************************
;;  Block write as individual records to a child repository.
;;  Note: Individual record writes, Yes transaction management.
;;************************************************************************
(writeln "22. Block write as individual records to a child repository.") 
(writeln "Note: Individual record writes, Yes transaction management.") 
(gc)(setq startTime (getTickCount 0))
(saveRepository db block: 2000000)
(beginTransaction (setq dbc db.block))
(loop for i from 0 until m do
   (setq dbc[i] dbblock[i])
   )
(commitTransaction dbc)
(setq endTime (getTickCount startTime))
(writeln "Timing test for block write as individual records to a child repository = " endTime " seconds.")
(inspect dbc check:)
(testit "(length dbc)" m)
(setq dbc #void)

;;************************************************************************
;;  Block read as individual records from a child repository.
;;  Note: Individual record reads, Yes transaction management.
;;************************************************************************
(writeln "23. Block read as individual records from a child repository.") 
(writeln "Note: Individual record reads, Yes transaction management.") 
(gc)(setq mbblock (loadRepository db.block))
(setq startTime (getTickCount 0))
(beginTransaction (setq dbc db.block))
(loop for i from 0 until m do
   (setq mbblock[i] dbc[i])
   ) ;; end of block read loop
(abortTransaction dbc)
(setq endTime (getTickCount startTime))
(writeln "Timing test for block read as individual records from a child repository = " endTime " seconds.")
(loop for i from 0 until m do
   (testit "mbblock[i]" dbblock[i])
   ) ;; end of block read loop

;;************************************************************************
;;  Block write as single record to a child repository.
;;  Note: Write block as a child repository, No transaction management.
;;************************************************************************
(writeln "24. Block write as single record to a child repository.") 
(writeln "Note: Write block as a child repository, No transaction management.") 
(setq startTime (getTickCount 0))
(saveRepository db block: dbblock)
(setq endTime (getTickCount startTime))
(writeln "Timing test for block write as single record to a child repository = " endTime " seconds.")
(inspect db.block check:)
(testit "(length db.block)" m)

;;************************************************************************
;;  Block read as single record from a child repository.
;;  Note: Read block as a child repository, No transaction management.
;;************************************************************************
(writeln "25. Block read as single record from a child repository.") 
(writeln "Note: Read block as a child repository, No transaction management.") 
(gc)(setq startTime (getTickCount 0))
(setq mbblock (loadRepository db.block))
(setq endTime (getTickCount startTime))
(writeln "Timing test for block read as single record from a child repository = " endTime " seconds.")
(loop for i from 0 until m do
   (testit "mbblock[i]" dbblock[i])
   ) ;; end of block read loop
(loop for i from 0 until m do
   (testit "db.block[i]" dbblock[i])
   ) ;; end of record read loop

;;************************************************************************
;;  Updating a block of n records of fixed length in a child Repository.
;;  Note: Update whole repository, No transaction management.
;;************************************************************************
(writeln "26. Updating a block of n records of fixed length in a child Repository.") 
(writeln "Note: Update child repository, No transaction management.") 
(gc)(setq startTime (getTickCount 0))
(saveRepository db block: dbblock 2)
(loop for i from 0 until m do
   (setq db.block[i] dbblock[i])
   ) ;; end of record read loop
(setq endTime (getTickCount startTime))
(writeln "Timing test for block update of a child repository = " endTime " seconds.")
(loop for i from 0 until m do
   (setq mbblock[i] db.block[i])
   (testit "mbblock[i]" dbblock[i])
   ) ;; end of block read loop

;;************************************************************************
;;  Block write as individual records to a honeycomb repository.
;;  Note: Individual record writes, Yes transaction management.
;;************************************************************************
(writeln "27. Block write as individual records to a honeycomb repository.") 
(writeln "Note: Individual record writes, Yes transaction management.") 
(setq hc (fileOpen _HCPathName 1 4))
(gc)(setq startTime (getTickCount 0))
(loop for i from 0 until m do
   (honeyWriteRecord hc i dbblock[i])
   ) ;; end of record loop
(setq endTime (getTickCount startTime))
(fileClose hc 1)
(writeln "Timing test for block write as individual records to a honeycomb repository = " endTime " seconds.")

;;************************************************************************
;;  Block read as individual records from a honeycomb repository.
;;  Note: Individual record reads, Yes transaction management.
;;************************************************************************
(writeln "28. Block read as individual records from a honeycomb repository.") 
(writeln "Note: Individual record reads, Yes transaction management.") 
(setq hc (fileOpen _HCPathName 0 4))
(gc)(setq startTime (getTickCount 0))
(testit "x" dbblock)
(setq endTime (getTickCount startTime))
(fileClose hc 1)
(writeln "Timing test for block read as individual records from a honeycomb repository = " endTime " seconds.")

;;************************************************************************
;;  Block write as single record to a honeycomb repository.
;;  Note: Single record write, Yes transaction management.
;;************************************************************************
(writeln "29. Block write as single record to a honeycomb repository.") 
(writeln "Note: Single record write, Yes transaction management.") 
(setq hc (fileOpen _HCPathName 1 4))
(gc)(setq startTime (getTickCount 0))
(writeln "(sizeof dbblock[0]) =" (sizeof dbblock[0]))
(honeyWriteBlock hc 0 dbblock[0]) ;;TM? What is dbblock[0] at this point?
(setq endTime (getTickCount startTime))
(fileClose hc 1)
(writeln "Timing test for block write as a single record to a honeycomb repository = " endTime " seconds.")

;;************************************************************************
;;  Block read as single record from a honeycomb repository.
;;  Note: Single record read, Yes transaction management.
;;************************************************************************
(writeln "30. Block read as single record from a honeycomb repository.") 
(writeln "Note: Single record read, Yes transaction management.") 
(setq hc (fileOpen _HCPathName 0 4))
(setq x (saveObject dbblock[0]))
(gc)(setq startTime (getTickCount 0))
(honeyReadBlock hc 0 x)
(setq endTime (getTickCount startTime))
(fileClose hc 1)
(writeln "Timing test for block read as a single record from a honeycomb repository = " endTime " seconds.")


;;************************************************************************
;;  Block read as single record from a honeycomb repository.
;;  Note: Single record read, No transaction management.
;;************************************************************************
(writeln "31. Exercise repository with tree index strategy on.") 
(writeln "Note: Single record read & write, No transaction management.") 
(gc)(setq startTime (getTickCount 0))
(setq repo (new ObjectRepository: "test.db" tclear:))
(setq repo.a #{dir| a: Test1: b: Test2:})
(setq repo[tree: b:] #{dir| c: Test10: db: Test00:})
(testit "repo.a" #{dir| a: Test1: b: Test2:})
(testit "repo[tree: b:]" #{dir| c: Test10: db: Test00:})
(testit "repo[tree: a: b:]" Test2:)
(setq repo[tree: a: b:] TestCCC:)
(testit "repo[tree: a: b:]" TestCCC:)
(testit "repo[query: tree:]" true)
(testit "repo[query: length:]" 2)
(setq repo #void)
(setq endTime (getTickCount startTime))
(writeln "Timing test for exercise repository with tree index strategy on = " endTime " seconds.")


;;************************************************************************
;;  Block read as single record from a honeycomb repository.
;;  Note: Single record read, Yes transaction management.
;;************************************************************************
(writeln "32. Exercise repository with tree index strategy on.") 
(writeln "Note: Single record read & write, No transaction management.") 
(gc)(setq startTime (getTickCount 0))
(setq repo (new ObjectRepository: "test.db" tclear:))
(beginTransaction repo)
(setq repo.a #{dir| a: Test1: b: Test2:})
(setq repo[tree: b:] #{dir| c: Test10: db: Test00:})
(testit "repo.a" #{dir| a: Test1: b: Test2:})
(testit "repo[tree: b:]" #{dir| c: Test10: db: Test00:})
(testit "repo[tree: a: b:]" Test2:)
(setq repo[tree: a: b:] TestCCC:)
(testit "repo[tree: a: b:]" TestCCC:)
(testit "repo[query: tree:]" true)
(testit "repo[query: length:]" 2)
(commitTransaction repo)
(setq repo #void)
(setq endTime (getTickCount startTime))
(writeln "Timing test for exercise repository with tree index strategy on = " endTime " seconds.")


/*************************************************************/
/*	Keyed Transactions, Transaction on						 */
/*************************************************************/
(define db (new ObjectRepository: _DBPathName clear:))
(clear db)
(setq db[0] "Hello world")
(setq m 300)
(setq startTime (getTickCount 0))
(beginTransaction db)
(loop for i from 0 until m do
    (testit "db[0]" "Hello world")
   )

(setq endTime (getTickCount startTime))
(abortTransaction db)
(writeln "Elaspsed Time with Keyed Transaction = " endTime " seconds")


/*************************************************************/
/*	Keyed Transactions, Transaction off					     */
/*************************************************************/
(define db (new ObjectRepository: _DBPathName ))
(clear db)
(setq db[0] "Hello world")
(setq m 300)
(setq startTime (getTickCount 0))

(loop for i from 0 until m do
    (testit "db[0]" "Hello world")
   )

(setq endTime (getTickCount startTime))
(writeln "Elaspsed Time keyed Transaction, begin Transaction off = " endTime  " seconds")

(testEnd "Test_ObjRepo.sl")
























































