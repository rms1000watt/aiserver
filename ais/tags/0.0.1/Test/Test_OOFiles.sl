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
;;  Title:    SmartLisp Object Files Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the Object Files
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_OOFiles.sl")


(define (timingTest proc count) vars:(startTime endTime)
    (setq startTime (getTickCount 0))
    (proc count) 
    (setq endTime (getTickCount startTime))
    (writeln "Elapsed time is " endTime " Seconds" ) endTime)

;;************************************************************************
;;   Creating Lambda for manging a repository of child repositories.
;;   Note: This Lambda is used in further repository tests.
;;************************************************************************
(defun rdbMS()
   pvars:(repo recID blockKEY blockREPO)
   ; Create a new relational repository.
   (defun create(name) (setq repo (new ObjectRepository: name clear:)))
   ; Open a relational repository.
   (defun open(name) (setq repo (new ObjectRepository: name)))
   ; Start a relational repository transaction.
   (defun start() 
      (beginTransaction repo)
      (setq blockREPO #void)
      (setq blockKEY #void)) ; end start
   ; Stop a relational repository transaction.
   (defun stop() 
      (if (<> blockREPO #void) 
          (begin
             (commitTransaction blockREPO)
             (setq blockREPO #void)
             (setq blockKEY #void)
             )) ; end if
      (commitTransaction repo)) ; end stop
   ; Write records sequentially into a blocked repository.
   (defun write(recno record)
      vars:(recKEY)
      ; Allocate a new block (if old one is #void).
      (if (= blockREPO #void)
          (begin
             (saveRepository repo (++ blockKEY) 500000)
             (setq blockREPO repo[blockKEY])
             (beginTransaction blockREPO)
             ;(writeln "New block: key=" blockKEY ",oid=" blockREPO ",free=" (inspect blockREPO free:))
             )) ; end if
      ; Allocate a new block (if old one is full).
      (if (or (> (length blockREPO) 500) (< (inspect blockREPO free:) 100000))
          (begin
             ;; Close previous block
             (commitTransaction blockREPO)
             ;; Allocate block
             (saveRepository repo (++ blockKEY) 500000)
             (setq blockREPO repo[blockKEY])
             (beginTransaction blockREPO)
             ;(writeln "New block: key=" blockKEY ",oid=" blockREPO ",free=" (inspect blockREPO free:))
             )) ; end if
      (setq recKEY (addi (muli 1000000 blockKEY) (modi recno 10000)))
      ; Save the record in the block
      (setq blockREPO[recKEY] record)
      ;(writeln "New record[" recno "]: key=" recKEY ",oid=" blockREPO ",free=" (inspect blockREPO free:))
      ; Return the record key so it can be found later  
      recKEY) ; end write
   ; Read records sequentially from a blocked repository.
   (defun read(recKEY)
      vars:(recKEY newBlockKEY record)
      ; Compute a new block key from the record key.
      (setq newBlockKEY (divi recKEY 1000000))
      ; Allocate a new block (if old one is #void).
      (if (= blockREPO #void)
          (begin
             (setq blockKEY newBlockKEY)
             (setq blockREPO repo[blockKEY])
             (beginTransaction blockREPO)
             ;(writeln "Load block: key=" blockKEY ",oid=" blockREPO ",free=" (inspect blockREPO free:))
             )) ; end if
      ; Allocate a new block (if old one is full).
      (if (<> newBlockKEY blockKEY)
          (begin
             ;; Close previous block
             (commitTransaction blockREPO)
             ;; Allocate block
             (setq blockKEY newBlockKEY)
             (setq blockREPO repo[blockKEY])
             (beginTransaction blockREPO)
             ;(writeln "Load block: key=" blockKEY ",oid=" blockREPO ",free=" (inspect blockREPO free:))
             )) ; end if
      ; Read the record from the block
      (setq record blockREPO[recKEY])
      ;(writeln "Read record: key=" recKEY ",oid=" blockREPO ",free=" (inspect blockREPO free:))
      ; Return the record.  
      record) ; end read
   true) ; end rdbMS

(define n 1000)
(define PAGESIZE 256)
(define PAGEROUND (sub1 PAGESIZE))

;;************************************************************************
;;  Start the Test Script
;;************************************************************************
(writeln   scriptName " started")

;;************************************************************************
;;   Starting test of findBlock and freeBlock bitmap functions.
;;   Note: This tests the bit map allocation/deallocation functions.
;;************************************************************************
(gc)
(writeln "starting test of findBlock and freeBlock bitmap functions.")
(loop for i from 1 until 25 do
    (setq v (new BitVector: 100 1))
    (loop for j from 0 until 75 do
        (freeBlock v j i)
        (setq y (findBlock v i))
        (testit "y" j)
        ) ; end deallocate loop
    ) ; end outter loop
(writeln "completing test of findBlock and freeBlock bitmap functions.")

;;************************************************************************
;;   Saving "n" records as objects at discrete file locations.
;;   Note: This tests the disk time.
;;************************************************************************
(gc)
(writeln "starting save of " n " records as objects at discrete file locations" )
(setq recids (new Vector: integer: n))
(setq blocks (new Vector: bit: (muli n 10)))
(setq record (new Vector: 200 22))
(setq startTime (getTickCount 0))
(setq fileID (fileOpen "cache.bin" 1 4))
(loop for i from 0 until n do
  (setq recBlocks (divi (+ (sizeof record) PAGEROUND) PAGESIZE))
  (setq recids[i] (muli (findBlock blocks recBlocks) PAGESIZE))
  (fileSeek fileID recids[i] 1)
  (saveObject fileID record)
  ) ; end loop
(fileClose fileID 1)
(setq endTime (getTickCount startTime))
(writeln "time required for save of " n " records as objects at discrete file locations is " endTime " Seconds" )
(writeln "time required for save of 1,000,000 records as objects at discrete file locations is " 
         (/ (* endTime (/ 1000000 n)) 3600)
         " Hours" )

;;************************************************************************
;;   Reading "n" records as objects from discrete file locations.
;;   Note: This tests the disk time.
;;************************************************************************
(gc)
(writeln "starting read of " n " records as objects from discrete file locations" )
(setq startTime (getTickCount 0))
(setq fileID (fileOpen "cache.bin" 0 4))
(loop for i from 0 until n do
  (fileSeek fileID recids[i] 1)
  (setq x (loadObject fileID))
  (testit "record" x)
  ) ; end loop
(fileClose fileID 1)
(setq endTime (getTickCount startTime))
(writeln "time required for read of " n " records as objects from discrete file locations is " endTime " Seconds" )
(writeln "time required for read of 1,000,000 records as objects from discrete file locations is " 
         (/ (* endTime (/ 1000000 n)) 3600)
         " Hours" )

;;************************************************************************
;;   Saving "n" compressed records as objects at discrete file locations.
;;   Note: This tests the disk time.
;;************************************************************************
(gc)
(writeln "starting save of " n " compressed records as objects at discrete file locations" )
(setq recids (new Vector: integer: n))
(setq blocks (new Vector: bit: (muli n 10)))
(setq record (new Vector: 200 22))
(setq startTime (getTickCount 0))
(setq fileID (fileOpen "ccache.bin" 1 4))
(loop for i from 0 until n do
  (setq y (compress (saveObject record)))
  (setq recBlocks (divi (+ (sizeof y) PAGEROUND) PAGESIZE))
  (setq recids[i] (muli (findBlock blocks recBlocks) PAGESIZE))
  (fileSeek fileID recids[i] 1)
  (saveObject fileID y)
  ) ; end loop
(fileClose fileID 1)
(setq endTime (getTickCount startTime))
(writeln "time required for save of " n " compressed records as objects at discrete file locations is " 
         endTime " Seconds" )
(writeln "time required for save of 1,000,000 compressed records as objects at discrete file locations is " 
         (/ (* endTime (/ 1000000 n)) 3600)
         " Hours" )

;;************************************************************************
;;   Reading "n" compressed records as objects from discrete file locations.
;;   Note: This tests the disk time.
;;************************************************************************
(gc)
(writeln "starting read of " n " compressed records as objects from discrete file locations" )
(setq startTime (getTickCount 0))
(setq fileID (fileOpen "ccache.bin" 0 4))
(loop for i from 0 until n do
  (fileSeek fileID recids[i] 1)
  (setq x (loadObject (uncompress (loadObject fileID))))
  (testit "record" x)
  ) ; end loop
(fileClose fileID 1)
(setq endTime (getTickCount startTime))
(writeln "time required for read of " n " compressed records as objects from discrete file locations is " 
         endTime " Seconds" )
(writeln "time required for read of 1,000,000 compressed records as objects from discrete file locations is " 
         (/ (* endTime (/ 1000000 n)) 3600)
         " Hours" )

;;************************************************************************
;;   Saving "n" records as objects into a repository.
;;   Note: This tests the disk time.
;;************************************************************************
(gc)
(writeln "starting save of " n " records as objects into a repository" )
(setq startTime (getTickCount 0))
(setq repo (new ObjectRepository: "cache.db" clear:))
(beginTransaction repo)
(loop for i from 0 until n do
  (setq repo[i] record)
  ) ; end loop
(commitTransaction repo)
(setq endTime (getTickCount startTime))
(writeln "time required for save of " n " records as objects into a repository is " endTime " Seconds" )
(writeln "time required for save of 1,000,000 records as objects into a repository is " 
         (/ (* endTime (/ 1000000 n)) 3600)
         " Hours" )

;;************************************************************************
;;   Reading "n" records as objects from a repository.
;;   Note: This tests the disk time.
;;************************************************************************
(gc)
(writeln "starting read of " n " records as objects from a repository" )
(setq startTime (getTickCount 0))
(setq repo (new ObjectRepository: "cache.db"))
(beginTransaction repo)
(loop for i from 0 until n do
  (setq x repo[i])
  (testit "record" x)
  ) ; end loop
(commitTransaction repo)
(setq endTime (getTickCount startTime))
(writeln "time required for read of " n " records as objects from a repository is " endTime " Seconds" )
(writeln "time required for read of 1,000,000 records as objects from a repository is " 
         (/ (* endTime (/ 1000000 n)) 3600)
         " Hours" )

;;************************************************************************
;;   Saving "n" records as objects at frame locations in a repository.
;;   Note: This tests the disk time.
;;************************************************************************
(gc)
(writeln "starting save of " n " records as objects at frame locations in a repository" )
(setq recids (new Vector: number: n))
(setq record (new Vector: 200 22))
(setq startTime (getTickCount 0))
(setq repo (new ObjectRepository: "fcache.db" clear:))
(beginTransaction repo)
(loop for i from 0 until n do
  (setq recids[i] (setq repo[frame: #void] record))
  ) ; end loop
(commitTransaction repo)
(setq endTime (getTickCount startTime))
(writeln "time required for save of " n " records as objects at frame locations is " endTime " Seconds" )
(writeln "time required for save of 1,000,000 records as objects at frame locations is " 
         (/ (* endTime (/ 1000000 n)) 3600)
         " Hours" )

;;************************************************************************
;;   Reading "n" records as objects from frame locations in a repository.
;;   Note: This tests the disk time.
;;************************************************************************
(gc)
(writeln "starting read of " n " records as objects from frame locations in a repository" )
(setq startTime (getTickCount 0))
(setq repo (new ObjectRepository: "fcache.db"))
(beginTransaction repo)
(loop for i from 0 until n do
  (setq x repo[frame: recids[i]])
  (testit "record" x)
  ) ; end loop
(abortTransaction repo)
(setq endTime (getTickCount startTime))
(writeln "time required for read of " n " records as objects from frame locations is " endTime " Seconds" )
(writeln "time required for read of 1,000,000 records as objects from frame  locations is " 
         (/ (* endTime (/ 1000000 n)) 3600)
         " Hours" )

;;************************************************************************
;;   Saving "n" records as objects into a repository of child repositories.
;;   Note: This tests the disk time for handling child repositories.
;;************************************************************************
(gc)
(writeln "starting save of " n " as objects into a repository of child repositories." )
(setq recids (new Vector: integer: n))
(setq startTime (getTickCount 0))
(rdbMS.create "ccache.db")
(rdbMS.start)
(loop for i from 0 until n do
  (setq recids[i] (rdbMS.write i record))
  ) ; end loop
(rdbMS.stop)
(setq endTime (getTickCount startTime))
(writeln "time required for save of " n " as objects into a repository of child repositories is " endTime " Seconds" )
(writeln "time required for save of 1,000,000 records as objects into a repository is " 
         (/ (* endTime (/ 1000000 n)) 3600)
         " Hours" )

;;************************************************************************
;;   Reading "n" records as objects from a repository of child repositories.
;;   Note: This tests the disk time for handling child repositories.
;;************************************************************************
(gc)
(writeln "starting read of " n " as objects from a repository of child repositories." )
(setq startTime (getTickCount 0))
(rdbMS.open "ccache.db")
(rdbMS.start)
(loop for i from 0 until n do
  (setq x (rdbMS.read recids[i]))
  (testit "x" record)
  ) ; end loop
(rdbMS.stop)
(setq endTime (getTickCount startTime))
(writeln "time required for read of " n " as objects from a repository of child repositories is " endTime " Seconds" )
(writeln "time required for read of 1,000,000 records as objects from a repository is " 
         (/ (* endTime (/ 1000000 n)) 3600)
         " Hours" )

;;************************************************************************
;;  Create the required Dimension variables.
;;************************************************************************
(gc)
(writeln "start creation of " n " dimension variables.")
(setq startTime (getTickCount 0))
(setq memFree (inspect))
(define x (makeVector number: n 1))
(setq endTime (getTickCount startTime))
(writeln "time required for creation of " n " dimension variables is " endTime " Seconds" )
(writeln "space required for creation of " n " dimension variables is " (- memFree (inspect)) " bytes")

;;************************************************************************
;;   Sum the dimension variable.
;;   Note: This tests the in memory time.
;;************************************************************************
(writeln "starting sum of " n " dimension variables" )
(setq startTime (getTickCount 0))
(sum x)
(setq endTime (getTickCount startTime))
(writeln "time required for sum of " n " dimension variables is " endTime " Seconds" )

;;************************************************************************
;;   Save the dimension variable.
;;   Note: This tests the disk time.
;;************************************************************************
(writeln "starting save of " n " dimension variables" )
(setq startTime (getTickCount 0))
(if (<> 0 (setq fileID (fileOpen "cache.bin" 1 4)))
 (begin
  (fileSeek fileID 0 1)
  (saveObject fileID x)
  (fileClose fileID 1)))
(setq endTime (getTickCount startTime))
(writeln "time required for save of " n " dimension variables is " endTime " Seconds" )

;;************************************************************************
;;   Sum the dimension variable.
;;   Note: This tests the disk time.
;;************************************************************************
(writeln "starting disk sum of " n " dimension variables" )
(setq x #void)
(setq startTime (getTickCount 0))
(if (<> 0 (setq fileID (fileOpen "cache.bin" 0 4)))
 (begin
  (setq x (loadObject fileID))
  (fileClose fileID 0)))
(sum x)
(setq endTime (getTickCount startTime))
(writeln "time required for disk sum of " n " dimension variables is " endTime " Seconds")

;;************************************************************************
;;  Delete all references to the vector to give back memory.
;;************************************************************************
(setq x #void)

(testEnd "Test_OOFiles.sl")



















