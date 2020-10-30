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
;;  Title:    SmartLisp Object Database Files Test
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  SmartBase Automated test suite 
;;
;;  Notes:    The SmartBase features in the Object Database Files
;;            chapter are tested in this test suite script.
;;
;;  Files:    No dependencies. 
;;

;;************************************************************************
;; Test script global variables
;;************************************************************************
(setq scriptName "Object Database Test")

;;************************************************************************
;;  Test Script diagnostic and utility functions
;;************************************************************************
(defun diagnostic(msg) (error "oodbms" msg))

(defun testit(evalTxt result)
   (setq lt evalTxt)
   (setq lr result)
   (if (not (isEqual (eval evalTxt) result ))
       (begin
         (writeln scriptName " *FAILURE* " evalTxt)
         (error "oodbms"))
       true))

(define (readTextFile name) 
   vars:(fileID self (type 0))
   (setq fileID (fileOpen name 0 type))
   (setq self (fileRead fileID))
   (fileClose fileID 1)
   self)

;;************************************************************************
;;  Start the Test Script
;;************************************************************************
(writeln   scriptName " started")

(define n 50)

;;************************************************************************
;;  Create the test Database.
;;************************************************************************
(setq dbindex (makeVector n -1))
(setq db (databaseOpen 0 "Test.db" new:))

;;************************************************************************
;;  Write n records of random length to the Database.
;;************************************************************************
(loop for i from 0 until n do
   (setq dbrecord (makeVector (addi (random n) 1) i))
   ;(writeln "starting 1 save [" i "] {" dbindex[i] "}")
   (setq dbindex[i] (databaseSave db dbrecord))
   ;(writeln "completed 1 save [" i "] {" dbindex[i] "}")
   ;(writeln "starting 1 load [" i "] {" dbindex[i] "}")
   (testit "(databaseLoad db dbindex[i])" dbrecord)
   ;(writeln "completed 1 load [" i "] {" dbindex[i] "}")
   )
(databaseSaveIndex db dbindex)
(databaseClose db commit:)

;;************************************************************************
;;  Read all Database records sequentially.
;;************************************************************************
;(writeln "starting to read live Database objects sequentially")
(setq xv (makeVector object: 0))
(setq db (databaseOpen 0 "Test.db" share:))
(setq objID -1)
(while (<> (setq objID (databaseNext db objID)) false) do
   (setq xv[objID] (databaseLoad db objID))
   ;(writeln "Sequentially read object [" objID "] {" dbrecord "}")
   )
(databaseClose db refuse:)

;;************************************************************************
;;  Update n records of random length, but refuse the transactions.
;;************************************************************************
(setq db (databaseOpen 0 "Test.db" update:))
(setq dbindex (databaseLoadIndex db))
(loop for i from 0 until n do
   (setq dbrecord (makeVector (addi (random n) 1) i))
   ;(writeln "starting 2 save [" i "] {" dbindex[i] "}")
   (setq dbindex[i] (databaseSave db dbrecord dbindex[i]))
   ;(writeln "completed 2 save [" i "] {" dbindex[i] "}")
   ;(writeln "starting 2 load [" i "] {" dbindex[i] "}")
   (testit "(databaseLoad db dbindex[i])" dbrecord)
   ;(writeln "completed 2 load [" i "] {" dbindex[i] "}")
   )
(databaseSaveIndex db dbindex)
(databaseClose db refuse:)

;;************************************************************************
;;  Read all Database records sequentially, and compare with previous.
;;************************************************************************
;(writeln "starting to compare live Database objects sequentially")
(setq db (databaseOpen 0 "Test.db" share:))
(setq objID -1)
(while (<> (setq objID (databaseNext db objID)) false) do
   (if (compareNE xv[objID] (databaseLoad db objID))
       (diagnostic "*FAILURE* of refuse transaction"))
   ;(writeln "Sequentially read object [" objID "] {" dbrecord "}")
   )
(databaseClose db refuse:)

;;************************************************************************
;;  Write n records of random length a second time with compression.
;;************************************************************************
(setq db (databaseOpen 0 "Test.db" update:))
(setq dbindex (databaseLoadIndex db))
(loop for i from 0 until n do
   (setq dbrecord (makeVector (addi (random n) 1) i))
   ;(writeln "starting 2 save [" i "] {" dbindex[i] "}")
   (setq dbindex[i] (databaseSave db dbrecord dbindex[i] true))
   ;(writeln "completed 2 save [" i "] {" dbindex[i] "}")
   ;(writeln "starting 2 load [" i "] {" dbindex[i] "}")
   (testit "(databaseLoad db dbindex[i] true)" dbrecord)
   ;(writeln "completed 2 load [" i "] {" dbindex[i] "}")
   )
(databaseSaveIndex db dbindex true)
(databaseClose db commit:)

;;************************************************************************
;;  Write n records of random length a third time with encryption.
;;************************************************************************
(setq key 123456789012345)
(setq db (databaseOpen 0 "Test.db" update:))
(setq dbindex (databaseLoadIndex db true))
(loop for i from 0 until n do
   (setq dbrecord (makeVector (addi (random n) 1) i))
   ;(writeln "starting 3 save [" i "] {" dbindex[i] "}")
   (setq dbindex[i] (databaseSave db dbrecord dbindex[i] key))
   ;(writeln "completed 3 save [" i "] {" dbindex[i] "}")
   ;(writeln "starting 3 load [" i "] {" dbindex[i] "}")
   (testit "(databaseLoad db dbindex[i] key)" dbrecord)
   ;(writeln "completed 3 load [" i "] {" dbindex[i] "}")
   )
(databaseSaveIndex db dbindex key)
(databaseClose db commit:)

;;************************************************************************
;;  Free every other record in the Database.
;;************************************************************************
(setq db (databaseOpen 0 "Test.db" update:))
(setq dbindex (databaseLoadIndex db key))
(loop for i from 0 until n by 2 do
   (setq dbindex[i] (databaseFree db dbindex[i]))
   )
(databaseSaveIndex db dbindex)
(databaseClose db commit:)

;;************************************************************************
;;  Write n records of random length a fourth time.
;;************************************************************************
(setq db (databaseOpen 0 "Test.db" update:))
(setq dbindex (databaseLoadIndex db))
(loop for i from 0 until n do
   (setq dbrecord (makeVector (addi (random n) 1) i))
   ;(writeln "starting 4 save [" i "] {" dbindex[i] "}")
   (setq dbindex[i] (databaseSave db dbrecord dbindex[i]))
   ;(writeln "completed 4 save [" i "] {" dbindex[i] "}")
   ;(writeln "starting 4 loade [" i "] {" dbindex[i] "}")
   (testit "(databaseLoad db dbindex[i])" dbrecord)
   ;(writeln "completed 4 load [" i "] {" dbindex[i] "}")
   )
(databaseSaveIndex db dbindex)
(databaseClose db erase:)

;;************************************************************************
;;  Test the compression/decompression algorithms.
;;************************************************************************

(setq x (readTextFile "test1.txt"))
(testit "(uncompress (compress x))" x)

;;************************************************************************
;;  Test the encode/decode algorithms.
;;************************************************************************

(setq x (readTextFile "test1.txt"))
(setq key 123456789012345)
(testit "(decode (encode x key) key)" x)

;;************************************************************************
;;  Create an ObjectRepository linked to "test.db".
;;************************************************************************

(define db (new ObjectRepository: "test.db" clear:))

(setq db[1] #(1))
(testit "(length db)" 1)
(setq db[2] #(1 2))
(testit "(length db)" 2)

(writeln   scriptName " completed")




