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
;;  Author:   Michael F. Korns
;;
;;  Project:  SmartBase Automated test suite 
;;
;;  Notes:    The SmartBase features in the Object Files
;;            chapter are tested in this test suite script.
;;
;;  Files:    No dependencies. 
;;

;;************************************************************************
;; Test script global variables
;;************************************************************************
(setq scriptName "Object Files Test")

;(setq repo (new ObjectRepository: "fcache.db" clear:))
;(saveRepository repo 1 #{dir| 1 "Large test record 001" 2 "Large test record 002" 3 "Large test record 003"})
;(saveRepository repo 1 500000)
;(inspect repo pages:)

;;************************************************************************
;;  Test Script diagnostic and utility functions
;;************************************************************************
(setq diagnostic writeln)

(defun testit(evalTxt result) 
   (setq lt evalTxt)
   (setq lr result)
   (if (not (isEqual (eval evalTxt) result ))
       (begin
         (diagnostic scriptName " *FAILURE* " evalTxt)
         (error "objectfiles"))
       true))

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
;;   Saving "n" records as objects at discrete file locations.
;;   Note: This tests the disk time.
;;************************************************************************
(gc)

(setq recids (new Vector: integer: n))
(setq blocks (new Vector: bit: (muli n 10)))
(setq record (new Vector: 200 22))

(return "ready")

(rdbMS.create "ccache.db")
(rdbMS.start)
(let ((r 0)) (setq recids[r] (rdbMS.write r record)))
(rdbMS.stop)
(inspect rdbMS.repo index:)
(inspect rdbMS.repo[1.0] index:)

















