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
;;testObjectPersist
;;	Aggressive test of saveObject and loadObject primitives
;; 	Author: 	Tim May
;;	Project:	AIS Regression Suite
;; 

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_ObjectPersist.sl")

(defun testObjectPersist()
	pvars:(
		test01
		test02
		test03
		test04
		test05
		test06
		(pFileName "ObjectBinarytestFile2.bin")
		)

(defun test01()
	(writeln "Test01 - test saving into a byte vector")
	vars:(aRecord aResult aResult2 aLength)

	;;Test saving immediate
	(setq aRecord 10) 
	(setq aResult (saveObject aRecord))
	(setq aLength (sizeof aRecord))
	(if (<> aLength (length aResult))
		(return (begin (writeln "Error: length wrong =" (length aResult) " " aResult) false)))
	(setq aResult2 (loadObject aResult))
	(if (<> aRecord aResult2)
		(return (begin (writeln "Error: bad compare") false)))

	;;Test saving object
	(setq aRecord (new Structure: a: 1 b: 2 c: 3))
	(setq aResult (saveObject aRecord))
	(setq aLength (sizeof aRecord))
	(if (<> aLength (length aResult))
		(begin (writeln "Error: length wrong =" (length aResult) " " aResult) (return false)))
	(setq aResult2 (loadObject aResult))
	(if (<> aRecord aResult2)
		(begin (writeln "Error: bad compare") (return false)))

true);test01

(defun test02()
	(writeln "Test02 - test saving a small object into a file")
	vars:(aFileId, aLength aRecord aRecord2)

	;;Test saving object
	(setq aRecord (new Structure: a: 1 b: 2 c: 3))
	(setq aFileId (fileOpen pFileName 1 4)) ;;Open new file for write
	(setq aLength (saveObject aFileId aRecord))
	(fileSeek aFileId 0 1) ;; Seek to beginning of file
	(setq aRecord2 (loadObject aFileId))
	(fileClose aFileId 1)
	(if (<> aRecord aRecord2)
		(begin (writeln "Error: bad compare")(return false))) ;;close and erase file
	
	;;Test saving immediate
	(setq aRecord 10)
	(setq aFileId (fileOpen pFileName 1 4)) ;;Open new file for write
	(setq aLength (saveObject aFileId aRecord))
	(fileSeek aFileId 0 1) ;; Seek to beginning of file
	(setq aRecord2 (loadObject aFileId))
	(fileClose aFileId 1)
	(if (<> aRecord aRecord2)
		(begin (writeln "Error: bad compare") (return false))) ;;close and erase file
true);test02

(defun test03()
	(writeln "Test03 - test saving a large object into a file")
	vars:(aFileId, aLength aRecord aRecord2 n)
	(setq aRecord (new Vector: number: 10000))
	(loop for n from 0 until 10000 do (setq aRecord[n] n))
	(setq aFileId (fileOpen pFileName 1 4)) ;;Open new file for write
	(setq aLength (saveObject aFileId aRecord))
	(fileSeek aFileId 0 1) ;; Seek to beginning of file
	(setq aRecord2 (loadObject aFileId))
	(fileClose aFileId 1) ;;close and erase file
	(if (<> aRecord aRecord2)
		(begin (writeln "Error: bad compare") (return false))) 
true);test03	

(defun test04()
	(writeln "Test04 - test saving an Lambda into a file")
	vars:(aFileId, aLength aRecord aRecord2)
	(setq aRecord (copy testObjectgPersist))
	(setq aFileId (fileOpen pFileName 1 4)) ;;Open new file for write
	(setq aLength (saveObject aFileId aRecord))
	(fileSeek aFileId 0 1) ;; Seek to beginning of file
	(setq aRecord2 (loadObject aFileId))
	(fileClose aFileId 1) ;; close and earase file
	(if (<> aRecord aRecord2)
		(begin (writeln "Error: bad compare") (return false))) ;;close and erase file
true);test04

(defun test05()
	(writeln "Test05 - test saving into an external memory buffer")
	vars:(aRecord aBufferSize apBufferPtr aRecord2 aRetLength)

	;;Test saving object
	(setq aRecord (new Structure: a: 1 b: 2 c: 3))
	(setq aBufferSize (sizeof aRecord))
	(setq apBufferPtr (createBuffer aBufferSize))
	(setq aRetLength (saveObject aRecord aBufferSize apBufferPtr))
	(setq aRecord2 (loadObject apBufferPtr))
	(deleteBuffer apBufferPtr)	
	(if (<> aRetLength aBufferSize)
		(begin (writeln "Error: length wrong " aRetLength " " aBufferSize) (return false)))
	(if (<> aRecord aRecord2)
		(begin (writeln "Error: bad compare")(return false)))

	;;Test saving immediate
	(setq aRecord 10)
	(setq aBufferSize (sizeof aRecord))
	(setq apBufferPtr (createBuffer aBufferSize))
	(setq aRetLength (saveObject aRecord aBufferSize apBufferPtr))
	(setq aRecord2 (loadObject apBufferPtr))
	(deleteBuffer apBufferPtr)	
	(if (<> aRetLength aBufferSize)
		(begin (writeln "Error: length wrong " aRetLength " " aBufferSize) (return false)))
	(if (<> aRecord aRecord2)
		(begin (writeln "Error: bad compare")(return false)))
true);test05

(defun test06()
	(writeln "Test06 - test saving multiple variable length closures into a file")
	vars:(aRecordVec aRecord2Vec aRetLength (aFilePos 0) aFilePosVec aFileId 
		j n (N 100) aLen)
	(setq aFilePosVec (new Vector: number: N))
	(setq aRecordVec (new Vector: N))
	(setq aRecord2Vec (new Vector: N))
	(setq aFileId (fileOpen pFileName 1 4)) ;;Open new file for read/write
	;;generate and write records
	(loop for n from 0 until N do 
		(setq aLen (integer (random 100)))
		(setq aRecordVec[n] (new Vector: number: (* aLen 10)))
		(loop for j from 0 until (* aLen 10) do (setq aRecordVec[n][j] j)); seed record
		(setq aFilePosVec[n] aFilePos) ;; Grab current record position in file
		(setq aRetLength (saveObject aFileId aRecordVec[n]))
		(setq aFilePos (+ aFilePos aRetLength))
	)
	;;read records back
	(loop for n from 0 until N do
		(fileSeek aFileId aFilePosVec[n] 1)
		(setq aRecord2Vec[n] (loadObject aFileId))
	)
	(fileClose aFileId 1) ;;close and erase file

	(if (<> aRecordVec aRecord2Vec)
		(begin (writeln "Error: bad compare")(return false)))

true);test06


(if (not (test01)) (return false))
(if (not (test02)) (return false))
(if (not (test03)) (return false))
(if (not (test04)) (return false))
(if (not (test05)) (return false))
(if (not (test06)) (return false))

true);end testObjectPersist

(testit {(testObjectPersist)} true)
(setq testObjectPersist #void) ;;Clear test from memory
(gc) ;;get ready for next test

(testEnd "Test_ObjectPersist.sl")
