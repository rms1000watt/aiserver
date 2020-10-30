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

;;testObjectRepo
;;Aggressive test of object repositories
;; Author: 		Tim May
;; Project:		AIS Regression Suite
;; 

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_ObjectRepo.sl")

(defun testObjectRepo()
	pvars:(
		;;Child Lambdas
		test01
		test02
		test03
		testHarness

		;;Variables
		(pRepoName "TestRepo.db")
		(pRecordSize 2000)
		(pBlockSize 300)
		(pN 100)
		)


;;testHarness
;;called by test01, test02
;;args
;;	a_Keys 		-- a vector of keys of N length
;;	a_Records 	-- a vector of values of N length
;;	aBuffering	-- Number of items to buffer
(defun testHarness(a_Keys a_Records aBuffering)
	vars:(aKeys aRecords aRecords2 j J n N c aRandomVec aRecord aDb)

	(setq aKeys (copy a_Keys))
	(setq aRecords (copy a_Records))

	(setq N (length aKeys))

	(setq aDb (new ObjectRepository: pRepoName clear: buffer: aBuffering))

	;;Test setf -- save immediate
	(loop for n from 0 until N do
		(setf aDb[aKeys[n]] aRecords[n])
	); end n

	;;Test length of repository
	(if (<> (length aDb) N)
		(begin (writeln "Error.10: Bad record count") (return false)))

	;;Read test on values stored with setf
	(loop for n from 0 until N do
		(if (<> aDb[aKeys[n]] aRecords[n])
			(begin (writeln "Error.20: Bad Compare") (return false)))
	)

	(clear aDb)

	;;Test setq -- save non immediate. Entires in the repository index point to 
	;;records in the repository.
	(loop for n from 0 until N do
		(setq aDb[aKeys[n]] aRecords[n])
	); end n

	;;Test length of repository
	(if (<> (length aDb) N)
		(begin (writeln "Error.30: Bad record count") (return false)))

	;;Read test on values stored with setq
	(loop for n from 0 until N do
		(if (<> aDb[aKeys[n]] aRecords[n])
			(begin (writeln "Error.40: Bad Compare") (return false)))
	)
	
	(clear aDb)

	;;Mixed random setf reads/writes 
	;;Random length records and random ordinal selected for insert
	(setq aRandomRec (new Vector: N))
	(loop for n from 0 until N do
		(setq j (integer (random N)))
		(setq aRandomRec[j] true)
		(setf aDb[aKeys[j]] aRecords[j]) ;; Note setf
	);end n
	;Now read and compare
	(loop for n from 0 until N do
		(if aRandomRec[n]
			(if (<> aDb[aKeys[n]] aRecords[n])
				(begin (writeln "Error.50: Bad compare") (return false))))
	);end n

	(clear aDb)

	;;Mixed random setq reads/writes 
	;;Random length records and random ordinal selected for insert
	(setq aRandomRec (new Vector: N))
	(loop for n from 0 until N do
		(setq j (integer (random N)))
		(setq aRandomRec[j] true)
		(setq aDb[aKeys[j]] aRecords[j]) ;; Note setq
	);end n
	;Now read and compare
	(loop for n from 0 until N do
		(if aRandomRec[n]
			(if (<> aDb[aKeys[n]] aRecords[n])
				(begin (writeln "Error.60: Bad compare") (return false))))
	);end n

	(clear aDb)

	;;Test random erase of records
	(loop for n from 0 until N do
		(setq aDb[aKeys[n]] aRecords[n])
	);
	(setq J (integer (/ N 3)))
	(setq c 0)
	(loop for j from 0 to J do
		(setq n (integer (random (- N c))))
		(++ c)
		(setq aDb[aKeys[n]] #void)
		(delete aKeys n)
		(delete aRecords n)
	);end j
	
	(if (<> (length aDb) (length aKeys))
		(begin (writeln "Error.70: Delete failed " (length aDb) " " (length aKeys)) (return false)))

	(setq N (length aDb))
	(loop for n from 0 until N do
		(if (<> aDb[aKeys[n]] aRecords[n])
			(begin (writeln "Error.80: Bad compare") (return false)))

	); end n

	(clear aDb)
	

	;;Test random update of records
	(setq aKeys (copy a_Keys))
	(setq aRecords (copy a_Records))
	(setq aRecords2 (copy (reverse aRecords))) ;create a different set of records for update
	(setq N (length aKeys))

	;;write all the records
	(loop for n from 0 until N do
		(setq aDb[aKeys[n]] aRecords[n])
	);end n

	(setq aRandomRec (copy aRecords))

	;;update some of the records	
	(setq J (integer (/ N 3))) ;update a third of the records
	(loop for j from 0 until J do
		(setq n (integer (random N)))
		(setq aRandomRec[n] aRecords2[n])
		(setq aDb[aKeys[n]] aRecords2[n])
	);end j
	
	;;compare all the records after update
	(loop for n from 0 until N do
		(if (<> aDb[aKeys[n]] aRandomRec[n])
			(begin (writeln "Error.90: Bad compare") (return false)))
	);end n

	(clear aDb)

	(beginTransaction aDb)
	(loop for n from 0 until N do
		(setq aDb[aKeys[n]] aRecords[n])
	);n
	(commitTransaction aDb)
	(loop for n from 0 until N do
		(if (<> aDb[aKeys[n]] aRecords[n])
			(begin (writeln "Error.100: Bad compare") (return false)))
	);n	

	(clear aDb)

	(beginTransaction aDb)
	(loop for n from 0 until N do
		(setq aDb[aKeys[n]] aRecords[n])
	);n
	(abortTransaction aDb)
	(if (<> (length aDb) 0)
		(begin (writeln "Error.110: abortTransaction failed") (return false)))

	(clear aDb)

	;;test renaming (changing key of) a record in repository
	;;Can not execute when buffering is active
	(if (= aBuffering 0) (begin
		(setq aDb[aKeys[0]] aRecords[0])
		(rename aDb aKeys[0] aKeys[1])
		(if (<> aDb[aKeys[1]] aRecords[0])
			(begin (writeln "Error.120: rename record failed") (return false)))
		(if (<> aDb[aKeys[0]] #void)
			(begin (writeln "Error.130: rename record failed") (return false)))
		))
	(clear aDb)

true);testHarness

(defun test01()
	(writeln "Test01 -- Fixed length records")
	vars:(aRecords aRecord aKeys aKey j J n N)
	;;Create a vector of fixed length number vectors
	;;Each number vector will be used as a record for testing
	(setq N pN)

	(setq aRecords (new Vector: N))
	(loop for n from 0 until N do
		(setq J 50)
		(setq aRecord (new Vector: number: J))
		(loop for j from 0 until J do
			(setq aRecord[j] (+ j n))
		);j
		(setq aRecords[n] aRecord)
	);end n

	;;build simple keys
	(setq aKeys (new Vector: N))
	(loop for n from 0 until N do
		(setq aKeys[n] n)
	);end n
	
	(writeln "       -- simple keys, with no buffering")
	(if (not (testHarness aKeys aRecords 0)) (return false))
	(writeln "       -- simple keys, with buffering")
	(if (not (testHarness aKeys aRecords 4)) (return false))

	;;build complex keys
	(setq aKeys (new Vector: N))
	(loop for n from 0 until N do
		(setq aKeys[n] (new Structure: a: n b: (- n 1)))
	);end n

	(writeln "       -- complex keys, no buffering")
	(if (not (testHarness aKeys aRecords 0)) (return false))

	(writeln "       -- complex keys, with buffering")
	(if (not (testHarness aKeys aRecords 4)) (return false))

true); test01


(defun test02 ()
	(writeln "Test02 -- Variable length records")
	vars:(aRecords aRecord aKeys aKey j J n N)

	;;Create a vector of random length number vectors
	;;Each number vector will be used as a record for testing
	(setq N pN)
	(setq aRecords (new Vector: N))
	(loop for n from 0 until N do
		(setq J (integer (random 50)))
		(setq aRecord (new Vector: number: J))
		(loop for j from 0 until J do
			(setq aRecord[j] (+ j n))
		);
		(setq aRecords[n] aRecord)
	);end n


	;;build simple keys
	(setq aKeys (new Vector: N))
	(loop for n from 0 until N do
		(setq aKeys[n] n)
	);end n


	(writeln "       -- simple keys, no buffering")
	(if (not (testHarness aKeys aRecords 0)) (return false))

	(writeln "       -- simple keys, with buffering")
	(if (not (testHarness aKeys aRecords 4)) (return false))

	;;build complex keys
	(setq aKeys (new Vector: N))
	(loop for n from 0 until N do
		(setq aKeys[n] (new Structure: a: n b: (- n 1)))
	);end n

	(writeln "       -- complex keys, no buffering")
	(if (not (testHarness aKeys aRecords 0)) (return false))

	(writeln "       -- complex keys, with buffering")
	(if (not (testHarness aKeys aRecords 4)) (return false))

true) ;test02

(defun test03()
	vars:(aDb aNd aNx i (M 30))

	(writeln "Test03 -- Perform general tests of child repositories and inspection of the Repository.") 
	(writeln "Note: No buffering, and no transaction management.")
	  
	(setq aDb (new ObjectRepository: pRepoName clear:)) 

	(setq aDb.x 22)
	(if (<> aDb.x 22) 
		(begin (writeln "Error.010: Repository Test Failed on db.x")(return false)))

	(saveRepository aDb y: 30000)
	(setq aDb.y.x 33)
	
	(if (<> aDb.y.x 33) 
		(begin (writeln "Error.020: Repository Test Failed on db.y.x")(return false)))

	(saveRepository aDb y: #{dir| a: #(1 2 3) b: #(4 5 6)})
	(if (<> aDb.y.a #(1 2 3))
		(begin (writeln "Error.030: Repository test failed")(return false)))
	(if (<> aDb.y.b #(4 5 6))
		(begin (writeln "Error.040: Repository test failed")(return false)))
	(if (<> (length aDb.y) 2)
		(begin (writeln "Error.050: Repository test failed")(return false)))
	
	(setq aNd (new Directory:))
	(loop for i from 0 until M do
	   (setq aNd[i] (new Vector: 3 1 2 3))
	   ) ;; end loop

	(saveRepository aDb y: aNd)

	(if (<> aDb.y[0][0] 1) 
		(begin (writeln "Error.060: Repository Test Failed on db.y.x")(return false)))

	(if (<> aDb.y[2][1] 2) 
		(begin (writeln "Error.070: Repository Test Failed on db.y.y[1]")(return false)))

	(setq aNx (loadRepository aDb.y))
	(if (<> aNx[0] aNd[0])
		(begin (writeln "Error.080: Repository Test Failed on nx.x")(return false)))

	(if (<> aNx[1] aNd[1]) 
		(begin (writeln "Error.090: Repository Test Failed on nx.y")(return false)))

	(clear aDb)
	

true);test03


(if (not (test01)) (return false))
(if (not (test02)) (return false))
(if (not (test03)) (return false))

true);testObjectRepo


;;************************************************************************
;;  Create a librarian Lambda for a Repository.
;;  Note: no buffering, and no transaction management.
;;************************************************************************
(defun libLambda(obr)	;; We are called here at new
    pvars:(myOR myIndex)
    (defun doClear() (clear myOR))
    (defun beginTrans() (beginTransaction myOR))
    (defun commitTrans() (commitTransaction myOR))
    (defun abortTrans() (abortTransaction myOR))
    (defun len() (length myOR))
    (defun ref1(ix1) myOR[ix1])
    (defun ref2(ix1 ix2) myOR[ix1 ix2])
    (defun set1(ix1 newValue) (setq myOR[ix1] newValue))
    (setq myOR obr)
    true) ;; end libLambda

(testit {(testObjectRepo)} true)
(setq testObjectRepo #void)
(setq libLambda #void )

(testEnd "Test_ObjectRepo.sl")


