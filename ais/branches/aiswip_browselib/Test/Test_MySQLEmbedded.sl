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
;;  Title:    MySQL Procedures Test
;;
;;  Author:   Franklin Chua
;;            Michael Korns
;;			  Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the SQL Procedures
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;; This test is primarily a Regression Suite test and as such will not test
;; functionality against a remote server. However, you can copy this test
;; and use it for UNIT testing against a remote server if desired. TM
;;
;; If you test a MySQL server instead of the embeded server then 
;; this test will require a working MySQL server installed the local machine,
;; and that the MySQL server can be accessed through "localhost".
;;
;; For configuration of the MySQL server, the minimum requirements include:
;; 1. Database user account (username=ais)
;; 2. Database user password (password=ais123)
;; 3. Working database (database=ais)
;; 4. Necessary access permissions for the ais user to the ais database
;;
;; Notes:
;; A privileged or administrator account is required to create a new user.
;; The following commands can be used to create the necessary MySQL user account:
;;
;; user@host:~> mysql -u root -p
;; Enter password:
;; mysql> GRANT ALL PRIVILEGES ON ais.* TO ais IDENTIFIED BY 'ais123'
;; mysql> quit
;;
;;
;; The administrator account or the ais user account can be used to create the ais database;
;; The following command can be used to create the ais database:
;;
;; user@host:~> mysql -u ais -p
;; Enter password: (ais123)
;; mysql> CREATE DATABASE ais
;; mysql> quit
;;
;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_MySQLEmbeded.sl")

;; Determine the correct architecture
(if (= _ais.bits 64) (setq archType "64-bit")(setq archType "32-bit"))

;; Set the test mode
;(setq useEmbedded false)
(setq useEmbedded true)


(defun comparefloat(float1 float2 tolerance)
	(< (abs (- float1 float2)) tolerance))
 
(defun randomstring(strlen)
	vars:(out bvec_i)
	(setq out (new ByteVector: strlen))
	(loop for bvec_i from 0 until strlen
		(setq out[bvec_i] (character (+ 33 (random 93))))
	)
	out)

(defun randombinstring(strlen)
	vars:(out bvec_i)
	(setq out (new ByteVector: strlen))
	(loop for bvec_i from 0 until strlen
		(setq out[bvec_i] (character (mod (random 512) 256)))
	)
	;; make sure that the first character is not 'A'
	;; this way we can ensure that we will not randomly generate AIS_ASC: or AIS_BIN:
	;; that will cause the engine to convert it to an AIS object
	(if (= out[bvec_i] #\A) (setq out[bvec_i] #\%))
	out)

(defun randombitstring(strlen)
	vars:(out bvec_i)
	(setq out (new BitVector: strlen))
	(loop for bvec_i from 0 until strlen
		(setq out[bvec_i] (integer (mod (random 64) 2)))
	)
	out)

;;************************************************************************
;; Start the Test
;;************************************************************************
(writeln scriptName " started")

;;************************************************************************
;; Define Test Parameters
;;************************************************************************
(setq hostTxt "localhost")
(setq userTxt "ais")
(setq passTxt "ais123")
(setq dbTxt "ais")

(setq hostStr (makeString hostTxt))
(setq userStr (makeString userTxt))
(setq passStr (makeString passTxt))
(setq dbStr   (makeString dbTxt))

(setq hostSym (makeSymbol hostTxt))
(setq userSym (makeSymbol userTxt))
(setq passSym (makeSymbol passTxt))
(setq dbSym   (makeSymbol dbTxt))

(setq FLOAT_TOLERANCE 0.0001)
(setq DOUBLE_TOLERANCE 0.00000001)

;;************************************************************************
;; MySQL Connectivity Tests (Out-of-Process MySQL Server)
;;************************************************************************
(if (not useEmbedded)
(begin
(testit "(type (setq sqlHndl (sql connect: hostStr #void userStr passStr)))" Integer:)
(testit "(sql disconnect: sqlHndl)" true)

;; Text
(testit "(type (setq sqlHndl (sql connect: hostTxt #void userTxt passTxt)))" Integer:)
(testit "(sql disconnect: sqlHndl)" true)

;; Symbol
(testit "(type (setq sqlHndl (sql connect: hostSym #void userSym passSym)))" Integer:)
(testit "(sql disconnect: sqlHndl)" true)
))

;;************************************************************************
;; MySQL Connectivity Tests (Embedded MySQL Server)
;;************************************************************************

;; SQL connect procedure parameters can be any of the following types:
;; String
(testit "(type (setq sqlHndl (sql connect:)))" Integer:)
(testit "(sql disconnect: sqlHndl)" true)

;; SQL connect with no parameter should return the same value everytime
(testit "(type (setq sqlHndl (sql connect:)))" Integer:)
(testit "(type (setq sqlHndl2 (sql connect:)))" Integer:)
(testit "(= sqlHndl sqlHndl2)" true)

;; SQL connect with no parameter sets the global context variable _DefaultSqlHandle
(testit "(= sqlHndl _defaultSqlHandle)" true)

;; SQL connect with -1 parameter should return a new handle
(testit "(type (setq sqlHndl3 (sql connect: -1)))" Integer:)
(testit "(type (setq sqlHndl4 (sql connect: -1)))" Integer:)
(testit "(type (setq sqlHndl5 (sql connect: -1)))" Integer:)

(testit "(= sqlHndl sqlHndl3)" false)
(testit "(= sqlHndl sqlHndl4)" false)
(testit "(= sqlHndl sqlHndl5)" false)
(testit "(= sqlHndl3 sqlHndl4)" false)
(testit "(= sqlHndl3 sqlHndl5)" false)
(testit "(= sqlHndl4 sqlHndl5)" false)

;;************************************************************************
;; MySQL Test Function
;;************************************************************************
(testit "(sql test: sqlHndl)" true)
(testit "(sql test: sqlHndl3)" true)
(testit "(sql test: sqlHndl4)" true)
(testit "(sql test: sqlHndl5)" true)

;; test should return false after we close the handle
(sql disconnect: sqlHndl)
(testit "(sql test: sqlHndl)" false)
(testit "(sql test: sqlHndl2)" false)

(sql disconnect: sqlHndl3)
(testit "(sql test: sqlHndl3)" false)

(sql disconnect: sqlHndl4)
(testit "(sql test: sqlHndl4)" false)

(sql disconnect: sqlHndl5)
(testit "(sql test: sqlHndl5)" false)

;;************************************************************************
;; MySQL Disconnect All Function
;;************************************************************************
;; Create a bunch of connections
(testit "(type (setq sqlHndl (sql connect:)))" Integer:)
(testit "(type (setq sqlHndl2 (sql connect: -1)))" Integer:)
(testit "(type (setq sqlHndl3 (sql connect: #void)))" Integer:)
(testit "(type (setq sqlHndl4 (sql connect: \"\")))" Integer:)

;; Make sure they all work
(testit "(sql test: sqlHndl)" true)
(testit "(sql test: sqlHndl2)" true)
(testit "(sql test: sqlHndl3)" true)
(testit "(sql test: sqlHndl4)" true)

;; Disconnect all handles
(sql disconnectall:)

;; All handles should be invalidated
(testit "(sql test: sqlHndl)" false)
(testit "(sql test: sqlHndl2)" false)
(testit "(sql test: sqlHndl3)" false)
(testit "(sql test: sqlHndl4)" false)

;;************************************************************************
;; MySQL Auto Connect Function
;;************************************************************************
;; Passing #void or 0 as the handle would automatically use the
;; default embedded connection
(testit "(type (sql connect:))" Integer:)
(testit "(isNumber (sql #void {DROP DATABASE IF EXISTS ais}))" true)
(testit "(sql #void {CREATE DATABASE IF NOT EXISTS ais})" 1)
(testit "(sql #void {USE ais})" 0)
(testit "(sql 0 {CREATE TABLE tmp1 (Id INT, Name VARCHAR(60))})" 0)
(setq recObj1 (new Brick: 10 Number:Integer:1))
(testit "(sql insert: 0 {tmp2} recObj1)" true)
(testit "(sql insert: #void {tmp3} recObj1)" true)
(testit "(sql 0 {DROP TABLE tmp1})" 0)
(testit "(sql 0 {DROP TABLE tmp2})" 0)
(testit "(sql 0 {DROP TABLE tmp3})" 0)
(testit "(sql 0 {DROP DATABASE ais})" 0)
(sql disconnect: (sql connect:))

;;************************************************************************
;; MySQL Info Function
;;************************************************************************
(setq sqlHndl (sql connect:))
(setq sqlHndl2 (sql connect: -1))
(setq sqlHndl3 (sql connect: -1))

(testit "(type (setq info (sql info: sqlHndl)))" Structure:)
(testit "(type (setq info (sql info: sqlHndl2)))" Structure:)
(testit "(type (setq info (sql info: sqlHndl3)))" Structure:)

(setq info #void)
(sql disconnectall:)

;;************************************************************************
;; MySQL Query Tests
;;************************************************************************
(if useEmbedded
(testit "(type (setq sqlHndl (sql connect:)))" Integer:)
else
(testit "(type (setq sqlHndl (sql connect: hostStr userStr passStr)))" Integer:)
)

;; Start with a clean Database
(testit "(type (sql sqlHndl {DROP DATABASE IF EXISTS } dbTxt))" Integer:)
(testit "(sql sqlHndl {CREATE DATABASE } dbTxt)" 1)
(testit "(sql sqlHndl {USE } dbTxt)" 0)

;;************************************************************************
;; Data Type (NULL VALUES) Handling Tests
;;************************************************************************
;; CREATE TABLE ...
(writeln "Creating Test Tables...")
(testit "(sql sqlHndl {CREATE TABLE table001 (col01 BIT)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table002 (col01 BIT(16))})" 0)
(testit "(sql sqlHndl {CREATE TABLE table003 (col01 BIT(32))})" 0)
(testit "(sql sqlHndl {CREATE TABLE table004 (col01 BIT(64))})" 0)
(testit "(sql sqlHndl {CREATE TABLE table005 (col01 TINYINT)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table006 (col01 TINYINT UNSIGNED)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table007 (col01 SMALLINT)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table008 (col01 SMALLINT UNSIGNED)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table009 (col01 MEDIUMINT)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table010 (col01 MEDIUMINT UNSIGNED)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table011 (col01 INT)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table012 (col01 INT UNSIGNED)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table013 (col01 BIGINT)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table014 (col01 BIGINT UNSIGNED)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table015 (col01 FLOAT)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table016 (col01 FLOAT UNSIGNED)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table017 (col01 DOUBLE)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table018 (col01 DOUBLE UNSIGNED)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table019 (col01 DECIMAL)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table020 (col01 DECIMAL UNSIGNED)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table021 (col01 DECIMAL(65))})" 0)
(testit "(sql sqlHndl {CREATE TABLE table022 (col01 DECIMAL(65,30))})" 0)
(testit "(sql sqlHndl {CREATE TABLE table023 (col01 DATE)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table024 (col01 DATETIME)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table025 (col01 TIMESTAMP)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table026 (col01 TIME)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table027 (col01 YEAR)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table028 (col01 CHAR)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table029 (col01 CHAR(10))})" 0)
(testit "(sql sqlHndl {CREATE TABLE table030 (col01 CHAR(255))})" 0)
(testit "(sql sqlHndl {CREATE TABLE table031 (col01 VARCHAR(1))})" 0)
(testit "(sql sqlHndl {CREATE TABLE table032 (col01 VARCHAR(32768))})" 0)
(testit "(sql sqlHndl {CREATE TABLE table033 (col01 BINARY)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table034 (col01 BINARY(255))})" 0)
(testit "(sql sqlHndl {CREATE TABLE table035 (col01 VARBINARY(1))})" 0)
(testit "(sql sqlHndl {CREATE TABLE table036 (col01 VARBINARY(32768))})" 0)
(testit "(sql sqlHndl {CREATE TABLE table037 (col01 TINYTEXT)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table038 (col01 TEXT)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table039 (col01 MEDIUMTEXT)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table040 (col01 LONGTEXT)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table041 (col01 TINYBLOB)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table042 (col01 BLOB)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table043 (col01 MEDIUMBLOB)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table044 (col01 LONGBLOB)})" 0)

;; Tests for handling empty rows
(writeln "Running Empty Row Tests...")
(testit "(sql sqlHndl {SELECT * FROM table001})" #void)
(testit "(sql sqlHndl {SELECT * FROM table002})" #void)
(testit "(sql sqlHndl {SELECT * FROM table003})" #void)
(testit "(sql sqlHndl {SELECT * FROM table004})" #void)
(testit "(sql sqlHndl {SELECT * FROM table005})" #void)
(testit "(sql sqlHndl {SELECT * FROM table006})" #void)
(testit "(sql sqlHndl {SELECT * FROM table007})" #void)

;; INSERT (NULL)
;; Tests for handling NULL values in different data types
(writeln "Running NULL Value Tests...")
(testit "(sql sqlHndl {INSERT INTO table001 VALUES (NULL)})" 1) ; BIT -> BitVector Object
(testit "(sql sqlHndl {INSERT INTO table005 VALUES (NULL)})" 1) ; TINYINT -> Short Type
(testit "(sql sqlHndl {INSERT INTO table009 VALUES (NULL)})" 1) ; MEDIUMINT -> Long Type
(testit "(sql sqlHndl {INSERT INTO table011 VALUES (NULL)})" 1) ; INT -> Long Type
(testit "(sql sqlHndl {INSERT INTO table015 VALUES (NULL)})" 1) ; FLOAT -> Float Type
(testit "(sql sqlHndl {INSERT INTO table017 VALUES (NULL)})" 1) ; DOUBLE -> Number Type
(testit "(sql sqlHndl {INSERT INTO table022 VALUES (NULL)})" 1) ; DECIMAL -> Money Type
(testit "(sql sqlHndl {INSERT INTO table023 VALUES (NULL)})" 1) ; DATE -> Date Type
(testit "(sql sqlHndl {INSERT INTO table028 VALUES (NULL)})" 1) ; CHARACTER -> Character Type
(testit "(sql sqlHndl {INSERT INTO table030 VALUES (NULL)})" 1) ; CHARACTER -> String Object
(testit "(sql sqlHndl {INSERT INTO table033 VALUES (NULL)})" 1) ; BINARY -> ByteVector Object
(testit "(sql sqlHndl {INSERT INTO table034 VALUES (NULL)})" 1) ; BINARY -> ByteVector Object

(testit "(type (setq result001 (sql sqlHndl {SELECT * FROM table001 ORDER BY col01})))" Brick:)
(testit "(type (setq result005 (sql sqlHndl {SELECT * FROM table005 ORDER BY col01})))" Brick:)
(testit "(type (setq result009 (sql sqlHndl {SELECT * FROM table009 ORDER BY col01})))" Brick:)
(testit "(type (setq result011 (sql sqlHndl {SELECT * FROM table011 ORDER BY col01})))" Brick:)
(testit "(type (setq result015 (sql sqlHndl {SELECT * FROM table015 ORDER BY col01})))" Brick:)
(testit "(type (setq result017 (sql sqlHndl {SELECT * FROM table017 ORDER BY col01})))" Brick:)
(testit "(type (setq result022 (sql sqlHndl {SELECT * FROM table022 ORDER BY col01})))" Brick:)
(testit "(type (setq result023 (sql sqlHndl {SELECT * FROM table023 ORDER BY col01})))" Brick:)
(testit "(type (setq result028 (sql sqlHndl {SELECT * FROM table028 ORDER BY col01})))" Brick:)
(testit "(type (setq result030 (sql sqlHndl {SELECT * FROM table030 ORDER BY col01})))" Brick:)
(testit "(type (setq result033 (sql sqlHndl {SELECT * FROM table033 ORDER BY col01})))" Brick:)
(testit "(type (setq result034 (sql sqlHndl {SELECT * FROM table034 ORDER BY col01})))" Brick:)

;; BIT
(testit "result001[\"RowCount\"]" 1)
(testit "(string result001[\"FieldList\"] true)" "#{col01: Object}")
(testit "result001[0][0]" #void)

;; TINYINT
(testit "result005[\"RowCount\"]" 1)
(testit "(string result005[\"FieldList\"] true)" "#{col01: Short}")
(testit "result005[0][0]" 0)

;; MEDIUMINT
(testit "result009[\"RowCount\"]" 1)
(testit "(string result009[\"FieldList\"] true)" "#{col01: Long}")
(testit "result009[0][0]" 0)

;; INT
(testit "result011[\"RowCount\"]" 1)
(testit "(string result011[\"FieldList\"] true)" "#{col01: Long}")
(testit "result011[0][0]" 0)

;; FLOAT
(testit "result015[\"RowCount\"]" 1)
(testit "(string result015[\"FieldList\"] true)" "#{col01: Float}")
(testit "result015[0][0]" 0)

;; DOUBLE
(testit "result017[\"RowCount\"]" 1)
(testit "(string result017[\"FieldList\"] true)" "#{col01: Number}")
(testit "result017[0][0]" 0.0)

;; DECIMAL
(testit "result022[\"RowCount\"]" 1)
(testit "(string result022[\"FieldList\"] true)" "#{col01: Money}")
(testit "result022[0][0]" $0.0)

;; DATE
(testit "result023[\"RowCount\"]" 1)
(testit "(string result023[\"FieldList\"] true)" "#{col01: Date}")
(testit "result023[0][0]" #Jan,1,0)

;; CHARACTER
(testit "result028[\"RowCount\"]" 1)
(testit "(string result028[\"FieldList\"] true)" "#{col01: Character}")
(testit "result028[0][0]" (character 0))

;; CHARACTER (Exceeds object header size, and gets converted to a String object)
(testit "result030[\"RowCount\"]" 1)
(testit "(string result030[\"FieldList\"] true)" "#{col01: Object}")
(testit "result030[0][0]" #void)

;; BINARY
(testit "result033[\"RowCount\"]" 1)
(testit "(string result033[\"FieldList\"] true)" "#{col01: Object}")
(testit "result033[0][0]" #void)

;; BINARY
(testit "result034[\"RowCount\"]" 1)
(testit "(string result034[\"FieldList\"] true)" "#{col01: Object}")
(testit "result034[0][0]" #void)

;; TRUNCATE ...
(writeln "Running Truncate Tests...")
(testit "(type (sql sqlHndl {TRUNCATE TABLE table001}))" Integer:)
(testit "(type (sql sqlHndl {TRUNCATE TABLE table005}))" Integer:)
(testit "(type (sql sqlHndl {TRUNCATE TABLE table009}))" Integer:)
(testit "(type (sql sqlHndl {TRUNCATE TABLE table011}))" Integer:)
(testit "(type (sql sqlHndl {TRUNCATE TABLE table015}))" Integer:)
(testit "(type (sql sqlHndl {TRUNCATE TABLE table017}))" Integer:)
(testit "(type (sql sqlHndl {TRUNCATE TABLE table022}))" Integer:)
(testit "(type (sql sqlHndl {TRUNCATE TABLE table023}))" Integer:)
(testit "(type (sql sqlHndl {TRUNCATE TABLE table028}))" Integer:)
(testit "(type (sql sqlHndl {TRUNCATE TABLE table030}))" Integer:)
(testit "(type (sql sqlHndl {TRUNCATE TABLE table033}))" Integer:)
(testit "(type (sql sqlHndl {TRUNCATE TABLE table034}))" Integer:)

;; Clean Result Values
(setq result001 #void)
(setq result005 #void)
(setq result009 #void)
(setq result011 #void)
(setq result015 #void)
(setq result017 #void)
(setq result022 #void)
(setq result023 #void)
(setq result028 #void)
(setq result030 #void)
(setq result033 #void)
(setq result034 #void)

;; Garbage Collection
(gc compact:)

;;************************************************************************
;; Data Type (NORMAL VALUES) Handling Tests
;;************************************************************************
;; INSERT (NORMAL VALUES)
(testit "(sql sqlHndl {INSERT INTO table001 VALUES (0), (1)})" 2)
(testit "(sql sqlHndl {INSERT INTO table002 VALUES (0), (1), (2), (65534), (65535)})" 5)
(testit "(sql sqlHndl {INSERT INTO table003 VALUES (0), (1), (2), (4294967294), (4294967295)})" 5)
(testit "(sql sqlHndl {INSERT INTO table004 VALUES (0), (1), (2), (18446744073709551614), (18446744073709551615)})" 5)
(testit "(sql sqlHndl {INSERT INTO table005 VALUES (-128), (-1), (0), (1), (127)})" 5)
(testit "(sql sqlHndl {INSERT INTO table006 VALUES (0), (1), (2), (254), (255)})" 5)
(testit "(sql sqlHndl {INSERT INTO table007 VALUES (-32768), (-1), (0), (1), (32767)})" 5)
(testit "(sql sqlHndl {INSERT INTO table008 VALUES (0), (1), (2), (32766), (32767)})" 5)
(testit "(sql sqlHndl {INSERT INTO table009 VALUES (-8388608), (-1), (0), (1), (8388607)})" 5)
(testit "(sql sqlHndl {INSERT INTO table010 VALUES (0), (1), (2), (16777214), (16777215)})" 5)
(testit "(sql sqlHndl {INSERT INTO table011 VALUES (-2147483648), (-1), (0), (1), (2147483647)})" 5)
(testit "(sql sqlHndl {INSERT INTO table012 VALUES (0), (1), (2), (2147483646), (2147483647)})" 5)
(testit "(sql sqlHndl {INSERT INTO table013 VALUES (-2147483648), (-1), (0), (1), (2147483647)})" 5)
(testit "(sql sqlHndl {INSERT INTO table014 VALUES (0), (1), (2), (2147483646), (2147483647)})" 5)
(testit "(sql sqlHndl {INSERT INTO table015 VALUES (-0.0000001), (-0.000001), (-0.00001), (-0.0001), (-0.001)})" 5)
(testit "(sql sqlHndl {INSERT INTO table015 VALUES (0.001), (0.0001), (0.00001), (0.000001), (0.0000001)})" 5)
(testit "(sql sqlHndl {INSERT INTO table016 VALUES (0.001), (0.0001), (0.00001), (0.000001), (0.0000001)})" 5)
(testit "(sql sqlHndl {INSERT INTO table016 VALUES (100.001), (10.0001), (1.00001), (0.100001), (0.000000100001)})" 5)
(testit "(sql sqlHndl {INSERT INTO table017 VALUES (-9999.2345), (-8888.3456), (-7777.4567), (-6666.5678), (-5555.6789)})" 5)
(testit "(sql sqlHndl {INSERT INTO table017 VALUES (9999.2345), (8888.3456), (7777.4567), (6666.5678), (5555.6789)})" 5)
(testit "(sql sqlHndl {INSERT INTO table018 VALUES (0.000000002345), (0.000000003456), (0.000000004567), (0.000000005678), (0.000000006789)})" 5)
(testit "(sql sqlHndl {INSERT INTO table018 VALUES (99990000.02345), (88880000.03456), (77770000.04567), (66660000.05678), (55550000.06789)})" 5)
(testit "(sql sqlHndl {INSERT INTO table019 VALUES (-9999999999), (-1), (0), (1), (9999999999)})" 5)
(testit "(sql sqlHndl {INSERT INTO table020 VALUES (0), (1), (5000000000), (9999999998), (9999999999)})" 5)
(testit "(sql sqlHndl {INSERT INTO table021 VALUES (-99999999999999999999999999999999999999999999999999999999999999999)})" 1)
(testit "(sql sqlHndl {INSERT INTO table021 VALUES (-1)})" 1)
(testit "(sql sqlHndl {INSERT INTO table021 VALUES (0)})" 1)
(testit "(sql sqlHndl {INSERT INTO table021 VALUES (1)})" 1)
(testit "(sql sqlHndl {INSERT INTO table021 VALUES (99999999999999999999999999999999999999999999999999999999999999999)})" 1)
(testit "(sql sqlHndl {INSERT INTO table022 VALUES (-99999999999999999999999999999999999.999999999999999999999999999999)})" 1)
(testit "(sql sqlHndl {INSERT INTO table022 VALUES (0)})" 1)
(testit "(sql sqlHndl {INSERT INTO table022 VALUES (99999999999999999999999999999999999.999999999999999999999999999999)})" 1)
(testit "(sql sqlHndl {INSERT INTO table023 VALUES ('1900-01-10'), ('1999-02-01'), ('2000-03-31'), ('2008-04-21'), ('2009-05-18')})" 5)
(testit "(sql sqlHndl {INSERT INTO table024 VALUES ('1900-01-01 00:00:01'), ('1999-12-31 23:59:59'), ('2000-01-01 00:00:01'), ('2008-12-31 23:59:59'), ('2009-01-01 00:00:01')})" 5)
(testit "(sql sqlHndl {INSERT INTO table025 VALUES ('1970-01-02 00:00:01'), ('1999-12-31 23:59:59'), ('2000-01-01 00:00:01'), ('2008-12-31 23:59:59'), ('2038-01-08 03:14:07')})" 5)
(testit "(sql sqlHndl {INSERT INTO table026 VALUES ('00:00:00'), ('00:00:01'), ('12:00:00'), ('23:59:58'), ('23:59:59')})" 5)
(testit "(sql sqlHndl {INSERT INTO table027 VALUES ('1901'), ('00'), ('70'), ('99'), ('69'), ('2155')})" 6)
(testit "(sql sqlHndl {INSERT INTO table028 VALUES ('a'), ('b'), ('c'), ('d'), ('e')})" 5)
(testit "(sql sqlHndl {INSERT INTO table028 VALUES ('0'), ('1'), ('2'), ('3'), ('4')})" 5)
(testit "(sql sqlHndl {INSERT INTO table028 VALUES ('Z'), ('Y'), ('X'), ('W'), ('V')})" 5)
(testit "(sql sqlHndl {INSERT INTO table029 VALUES (''), ('a'), (' abc'), ('abc '), (' abc '), ('abcdefghij')})" 6)
(testit "(sql sqlHndl {INSERT INTO table030 VALUES (''), ('a'), (' abc'), ('abc '), (' abc '), ('abcdefghijklmnopqrstuvwxyz')})" 6)
(testit "(sql sqlHndl {INSERT INTO table031 VALUES ('a'), ('b'), ('c'), ('d'), ('e')})" 5)
(testit "(sql sqlHndl {INSERT INTO table032 VALUES (''), ('a'), (' abc'), ('abc '), (' abc '), ('abcdefghijklmnopqrstuvwxyz')})" 6)
;; As of the moment, fields are LIMITED to non-binary values during INSERT
(testit "(sql sqlHndl {INSERT INTO table033 VALUES ('a'), ('b'), ('c'), ('d'), ('e')})" 5)
(testit "(sql sqlHndl {INSERT INTO table034 VALUES ('} (rept \"abcde\" 51) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table035 VALUES ('!'), ('@'), ('#'), ('$'), ('%')})" 5)
(testit "(sql sqlHndl {INSERT INTO table036 VALUES ('!@#$%^&*()'), ('qwertyuiop'), ('asdfghjkl'), ('zxcvbnm'), ('1234567890')})" 5)
(testit "(sql sqlHndl {INSERT INTO table037 VALUES ('} (rept \"0\" 200) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table037 VALUES ('} (rept \"1\" 200) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table037 VALUES ('} (rept \"2\" 200) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table037 VALUES ('} (rept \"3\" 200) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table037 VALUES ('} (rept \"4\" 200) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table038 VALUES ('} (rept \"0\" 400) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table038 VALUES ('} (rept \"1\" 400) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table038 VALUES ('} (rept \"2\" 400) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table038 VALUES ('} (rept \"3\" 400) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table038 VALUES ('} (rept \"4\" 400) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table039 VALUES ('} (rept \"0\" 800) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table039 VALUES ('} (rept \"1\" 800) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table039 VALUES ('} (rept \"2\" 800) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table039 VALUES ('} (rept \"3\" 800) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table039 VALUES ('} (rept \"4\" 800) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table040 VALUES ('} (rept \"0\" 1600) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table040 VALUES ('} (rept \"1\" 1600) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table040 VALUES ('} (rept \"2\" 1600) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table040 VALUES ('} (rept \"3\" 1600) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table040 VALUES ('} (rept \"4\" 1600) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table041 VALUES ('} (rept \"abcde\" 40) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table041 VALUES ('} (rept \"fghij\" 40) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table041 VALUES ('} (rept \"klmno\" 40) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table041 VALUES ('} (rept \"pqrst\" 40) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table041 VALUES ('} (rept \"uvwxy\" 40) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table042 VALUES ('} (rept \"abcde\" 80) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table042 VALUES ('} (rept \"fghij\" 80) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table042 VALUES ('} (rept \"klmno\" 80) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table042 VALUES ('} (rept \"pqrst\" 80) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table042 VALUES ('} (rept \"uvwxy\" 80) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table043 VALUES ('} (rept \"abcde\" 160) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table043 VALUES ('} (rept \"fghij\" 160) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table043 VALUES ('} (rept \"klmno\" 160) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table043 VALUES ('} (rept \"pqrst\" 160) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table043 VALUES ('} (rept \"uvwxy\" 160) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table044 VALUES ('} (rept \"abcde\" 320) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table044 VALUES ('} (rept \"fghij\" 320) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table044 VALUES ('} (rept \"klmno\" 320) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table044 VALUES ('} (rept \"pqrst\" 320) {')})" 1)
(testit "(sql sqlHndl {INSERT INTO table044 VALUES ('} (rept \"uvwxy\" 320) {')})" 1)

;; SELECT (ASCENDING)
;; Check if the values were inserted correctly
(testit "(type (setq result001 (sql sqlHndl {SELECT * FROM table001 ORDER BY col01})))" Brick:)
(testit "(type (setq result002 (sql sqlHndl {SELECT * FROM table002 ORDER BY col01})))" Brick:)
(testit "(type (setq result003 (sql sqlHndl {SELECT * FROM table003 ORDER BY col01})))" Brick:)
(testit "(type (setq result004 (sql sqlHndl {SELECT * FROM table004 ORDER BY col01})))" Brick:)
(testit "(type (setq result005 (sql sqlHndl {SELECT * FROM table005 ORDER BY col01})))" Brick:)
(testit "(type (setq result006 (sql sqlHndl {SELECT * FROM table006 ORDER BY col01})))" Brick:)
(testit "(type (setq result007 (sql sqlHndl {SELECT * FROM table007 ORDER BY col01})))" Brick:)
(testit "(type (setq result008 (sql sqlHndl {SELECT * FROM table008 ORDER BY col01})))" Brick:)
(testit "(type (setq result009 (sql sqlHndl {SELECT * FROM table009 ORDER BY col01})))" Brick:)
(testit "(type (setq result010 (sql sqlHndl {SELECT * FROM table010 ORDER BY col01})))" Brick:)
(testit "(type (setq result011 (sql sqlHndl {SELECT * FROM table011 ORDER BY col01})))" Brick:)
(testit "(type (setq result012 (sql sqlHndl {SELECT * FROM table012 ORDER BY col01})))" Brick:)
(if (= archType "64-bit")
(begin
(testit "(type (setq result013 (sql sqlHndl {SELECT * FROM table013 ORDER BY col01})))" Brick:)
(testit "(type (setq result014 (sql sqlHndl {SELECT * FROM table014 ORDER BY col01})))" Brick:)
))
(testit "(type (setq result015 (sql sqlHndl {SELECT * FROM table015 ORDER BY col01})))" Brick:)
(testit "(type (setq result016 (sql sqlHndl {SELECT * FROM table016 ORDER BY col01})))" Brick:)
(testit "(type (setq result017 (sql sqlHndl {SELECT * FROM table017 ORDER BY col01})))" Brick:)
(testit "(type (setq result018 (sql sqlHndl {SELECT * FROM table018 ORDER BY col01})))" Brick:)
(testit "(type (setq result019 (sql sqlHndl {SELECT * FROM table019 ORDER BY col01})))" Brick:)
(testit "(type (setq result020 (sql sqlHndl {SELECT * FROM table020 ORDER BY col01})))" Brick:)
(testit "(type (setq result021 (sql sqlHndl {SELECT * FROM table021 ORDER BY col01})))" Brick:)
(testit "(type (setq result022 (sql sqlHndl {SELECT * FROM table022 ORDER BY col01})))" Brick:)
(testit "(type (setq result023 (sql sqlHndl {SELECT * FROM table023 ORDER BY col01})))" Brick:)
(testit "(type (setq result024 (sql sqlHndl {SELECT * FROM table024 ORDER BY col01})))" Brick:)
(testit "(type (setq result025 (sql sqlHndl {SELECT * FROM table025 ORDER BY col01})))" Brick:)
(testit "(type (setq result026 (sql sqlHndl {SELECT * FROM table026 ORDER BY col01})))" Brick:)
(testit "(type (setq result027 (sql sqlHndl {SELECT * FROM table027 ORDER BY col01})))" Brick:)
(testit "(type (setq result028 (sql sqlHndl {SELECT * FROM table028 ORDER BY col01})))" Brick:)
(testit "(type (setq result029 (sql sqlHndl {SELECT * FROM table029 ORDER BY col01})))" Brick:)
(testit "(type (setq result030 (sql sqlHndl {SELECT * FROM table030 ORDER BY col01})))" Brick:)
(testit "(type (setq result031 (sql sqlHndl {SELECT * FROM table031 ORDER BY col01})))" Brick:)
(testit "(type (setq result032 (sql sqlHndl {SELECT * FROM table032 ORDER BY col01})))" Brick:)
(testit "(type (setq result033 (sql sqlHndl {SELECT * FROM table033 ORDER BY col01})))" Brick:)
(testit "(type (setq result034 (sql sqlHndl {SELECT * FROM table034 ORDER BY col01})))" Brick:)
(testit "(type (setq result035 (sql sqlHndl {SELECT * FROM table035 ORDER BY col01})))" Brick:)
(testit "(type (setq result036 (sql sqlHndl {SELECT * FROM table036 ORDER BY col01})))" Brick:)
(testit "(type (setq result037 (sql sqlHndl {SELECT * FROM table037 ORDER BY col01})))" Brick:)
(testit "(type (setq result038 (sql sqlHndl {SELECT * FROM table038 ORDER BY col01})))" Brick:)
(testit "(type (setq result039 (sql sqlHndl {SELECT * FROM table039 ORDER BY col01})))" Brick:)
(testit "(type (setq result040 (sql sqlHndl {SELECT * FROM table040 ORDER BY col01})))" Brick:)
(testit "(type (setq result041 (sql sqlHndl {SELECT * FROM table041 ORDER BY col01})))" Brick:)
(testit "(type (setq result042 (sql sqlHndl {SELECT * FROM table042 ORDER BY col01})))" Brick:)
(testit "(type (setq result043 (sql sqlHndl {SELECT * FROM table043 ORDER BY col01})))" Brick:)
(testit "(type (setq result044 (sql sqlHndl {SELECT * FROM table044 ORDER BY col01})))" Brick:)

;; Results for table001
(testit "result001[\"RowCount\"]" 2)
(testit "(string result001[\"FieldList\"] true)" "#{col01: Object}")

(testit "(type result001[0][0])" BitVector:)
(testit "(type result001[1][0])" BitVector:)

(testit "(string result001[0][0] true)" "#(bit| 0 0 0 0 0 0 0 0 )")
(testit "(string result001[1][0] true)" "#(bit| 0 0 0 0 0 0 0 1 )")

;; Results for table002
(testit "result002[\"RowCount\"]" 5)
(testit "(string result002[\"FieldList\"] true)" "#{col01: Object}")

(testit "(type result002[0][0])" BitVector:)
(testit "(type result002[1][0])" BitVector:)
(testit "(type result002[2][0])" BitVector:)
(testit "(type result002[3][0])" BitVector:)
(testit "(type result002[4][0])" BitVector:)

(testit "(string result002[0][0] true)" "#(bit| 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 )")
(testit "(string result002[1][0] true)" "#(bit| 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 )")
(testit "(string result002[2][0] true)" "#(bit| 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 )")
(testit "(string result002[3][0] true)" "#(bit| 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 )")
(testit "(string result002[4][0] true)" "#(bit| 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 )")

;; Results for table003
(testit "result003[\"RowCount\"]" 5)
(testit "(string result003[\"FieldList\"] true)" "#{col01: Object}")

(testit "(type result003[0][0])" BitVector:)
(testit "(type result003[1][0])" BitVector:)
(testit "(type result003[2][0])" BitVector:)
(testit "(type result003[3][0])" BitVector:)
(testit "(type result003[4][0])" BitVector:)

(testit "(string result003[0][0] true)" "#(bit| 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 )")
(testit "(string result003[1][0] true)" "#(bit| 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 )")
(testit "(string result003[2][0] true)" "#(bit| 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 )")
(testit "(string result003[3][0] true)" "#(bit| 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 )")
(testit "(string result003[4][0] true)" "#(bit| 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 )")

;; Results for table004
(testit "result004[\"RowCount\"]" 5)
(testit "(string result004[\"FieldList\"] true)" "#{col01: Object}")

(testit "(type result004[0][0])" BitVector:)
(testit "(type result004[1][0])" BitVector:)
(testit "(type result004[2][0])" BitVector:)
(testit "(type result004[3][0])" BitVector:)
(testit "(type result004[4][0])" BitVector:)

(testit "(string result004[0][0] true)" "#(bit| 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 )")
(testit "(string result004[1][0] true)" "#(bit| 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 )")
(testit "(string result004[2][0] true)" "#(bit| 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 )")
(testit "(string result004[3][0] true)" "#(bit| 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 )")
(testit "(string result004[4][0] true)" "#(bit| 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 )")

;; Results for table005
(testit "result005[\"RowCount\"]" 5)
(testit "(string result005[\"FieldList\"] true)" "#{col01: Short}")

(testit "(type result005[0][0])" Integer:)
(testit "(type result005[1][0])" Integer:)
(testit "(type result005[2][0])" Integer:)
(testit "(type result005[3][0])" Integer:)
(testit "(type result005[4][0])" Integer:)

(testit "result005[0][0]" -128)
(testit "result005[1][0]" -1)
(testit "result005[2][0]" 0)
(testit "result005[3][0]" 1)
(testit "result005[4][0]" 127)

;; Results for table006
(testit "result006[\"RowCount\"]" 5)
(testit "(string result006[\"FieldList\"] true)" "#{col01: Short}")

(testit "(type result006[0][0])" Integer:)
(testit "(type result006[1][0])" Integer:)
(testit "(type result006[2][0])" Integer:)
(testit "(type result006[3][0])" Integer:)
(testit "(type result006[4][0])" Integer:)

(testit "result006[0][0]" 0)
(testit "result006[1][0]" 1)
(testit "result006[2][0]" 2)
(testit "result006[3][0]" 254)
(testit "result006[4][0]" 255)

;; Results for table007
(testit "result007[\"RowCount\"]" 5)
(testit "(string result007[\"FieldList\"] true)" "#{col01: Short}")

(testit "(type result007[0][0])" Integer:)
(testit "(type result007[1][0])" Integer:)
(testit "(type result007[2][0])" Integer:)
(testit "(type result007[3][0])" Integer:)
(testit "(type result007[4][0])" Integer:)

(testit "result007[0][0]" -32768)
(testit "result007[1][0]" -1)
(testit "result007[2][0]" 0)
(testit "result007[3][0]" 1)
(testit "result007[4][0]" 32767)

;; Results for table008
(testit "result008[\"RowCount\"]" 5)
(testit "(string result008[\"FieldList\"] true)" "#{col01: Short}")

(testit "(type result008[0][0])" Integer:)
(testit "(type result008[1][0])" Integer:)
(testit "(type result008[2][0])" Integer:)
(testit "(type result008[3][0])" Integer:)
(testit "(type result008[4][0])" Integer:)

(testit "result008[0][0]" 0)
(testit "result008[1][0]" 1)
(testit "result008[2][0]" 2)
(testit "result008[3][0]" 32766)
(testit "result008[4][0]" 32767)

;; Results for table009
(testit "result009[\"RowCount\"]" 5)
(testit "(string result009[\"FieldList\"] true)" "#{col01: Long}")

(testit "(type result009[0][0])" Integer:)
(testit "(type result009[1][0])" Integer:)
(testit "(type result009[2][0])" Integer:)
(testit "(type result009[3][0])" Integer:)
(testit "(type result009[4][0])" Integer:)

(testit "result009[0][0]" -8388608)
(testit "result009[1][0]" -1)
(testit "result009[2][0]" 0)
(testit "result009[3][0]" 1)
(testit "result009[4][0]" 8388607)

;; Results for table010
(testit "result010[\"RowCount\"]" 5)
(testit "(string result010[\"FieldList\"] true)" "#{col01: Long}")

(testit "(type result010[0][0])" Integer:)
(testit "(type result010[1][0])" Integer:)
(testit "(type result010[2][0])" Integer:)
(testit "(type result010[3][0])" Integer:)
(testit "(type result010[4][0])" Integer:)

(testit "result010[0][0]" 0)
(testit "result010[1][0]" 1)
(testit "result010[2][0]" 2)
(testit "result010[3][0]" 16777214)
(testit "result010[4][0]" 16777215)

;; Results for table011
(testit "result011[\"RowCount\"]" 5)
(testit "(string result011[\"FieldList\"] true)" "#{col01: Long}")

(testit "(type result011[0][0])" Integer:)
(testit "(type result011[1][0])" Integer:)
(testit "(type result011[2][0])" Integer:)
(testit "(type result011[3][0])" Integer:)
(testit "(type result011[4][0])" Integer:)

(testit "result011[0][0]" -2147483648)
(testit "result011[1][0]" -1)
(testit "result011[2][0]" 0)
(testit "result011[3][0]" 1)
(testit "result011[4][0]" 2147483647)

;; Results for table012
(testit "result012[\"RowCount\"]" 5)
(testit "(string result012[\"FieldList\"] true)" "#{col01: Long}")
(testit "(type result012[0][0])" Integer:)
(testit "(type result012[1][0])" Integer:)
(testit "(type result012[2][0])" Integer:)
(testit "(type result012[3][0])" Integer:)
(testit "(type result012[4][0])" Integer:)

(testit "result012[0][0]" 0)
(testit "result012[1][0]" 1)
(testit "result012[2][0]" 2)
(testit "result012[3][0]" 2147483646)
(testit "result012[4][0]" 2147483647)

;; Results for table013
(if (= archType "64-bit")
(begin
(testit "result013[\"RowCount\"]" 5)
(testit "(string result013[\"FieldList\"] true)" "#{col01: Integer}")

(testit "(type result013[0][0])" Integer:)
(testit "(type result013[1][0])" Integer:)
(testit "(type result013[2][0])" Integer:)
(testit "(type result013[3][0])" Integer:)
(testit "(type result013[4][0])" Integer:)

(testit "result013[0][0]" -2147483648)
(testit "result013[1][0]" -1)
(testit "result013[2][0]" 0)
(testit "result013[3][0]" 1)
(testit "result013[4][0]" 2147483647)

;; Results for table014
(testit "result014[\"RowCount\"]" 5)
(testit "(string result014[\"FieldList\"] true)" "#{col01: Integer}")

(testit "(type result014[0][0])" Integer:)
(testit "(type result014[1][0])" Integer:)
(testit "(type result014[2][0])" Integer:)
(testit "(type result014[3][0])" Integer:)
(testit "(type result014[4][0])" Integer:)

(testit "result014[0][0]" 0)
(testit "result014[1][0]" 1)
(testit "result014[2][0]" 2)
(testit "result014[3][0]" 2147483646)
(testit "result014[4][0]" 2147483647)
))

;; Results for table015
(testit "result015[\"RowCount\"]" 10)
(testit "(string result015[\"FieldList\"] true)" "#{col01: Float}")

(testit "(type result015[0][0])" Number:)
(testit "(type result015[1][0])" Number:)
(testit "(type result015[2][0])" Number:)
(testit "(type result015[3][0])" Number:)
(testit "(type result015[4][0])" Number:)
(testit "(type result015[5][0])" Number:)
(testit "(type result015[6][0])" Number:)
(testit "(type result015[7][0])" Number:)
(testit "(type result015[8][0])" Number:)
(testit "(type result015[9][0])" Number:)

;; For Float tests, make sure that the difference from the actual does not exceed our tolerance value
(testit "(comparefloat -0.001 result015[0][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat -0.0001 result015[1][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat -0.00001 result015[2][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat -0.000001 result015[3][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat -0.0000001 result015[4][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat 0.0000001 result015[5][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat 0.000001 result015[6][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat 0.00001 result015[7][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat 0.0001 result015[8][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat 0.001 result015[9][0] FLOAT_TOLERANCE)" true)

;; Results for table016
(testit "result016[\"RowCount\"]" 10)
(testit "(string result016[\"FieldList\"] true)" "#{col01: Float}")

(testit "(type result016[0][0])" Number:)
(testit "(type result016[1][0])" Number:)
(testit "(type result016[2][0])" Number:)
(testit "(type result016[3][0])" Number:)
(testit "(type result016[4][0])" Number:)
(testit "(type result016[5][0])" Number:)
(testit "(type result016[6][0])" Number:)
(testit "(type result016[7][0])" Number:)
(testit "(type result016[8][0])" Number:)
(testit "(type result016[9][0])" Number:)

;; For Float tests, make sure that the difference from the actual does not exceed our tolerance value
(testit "(comparefloat 0.000000100001 result016[0][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat 0.0000001 result016[1][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat 0.000001 result016[2][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat 0.00001 result016[3][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat 0.0001 result016[4][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat 0.001 result016[5][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat 0.100001 result016[6][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat 1.00001 result016[7][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat 10.0001 result016[8][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat 100.001 result016[9][0] FLOAT_TOLERANCE)" true)

;; Results for table017
(testit "result017[\"RowCount\"]" 10)
(testit "(string result017[\"FieldList\"] true)" "#{col01: Number}")

(testit "(type result017[0][0])" Number:)
(testit "(type result017[1][0])" Number:)
(testit "(type result017[2][0])" Number:)
(testit "(type result017[3][0])" Number:)
(testit "(type result017[4][0])" Number:)
(testit "(type result017[5][0])" Number:)
(testit "(type result017[6][0])" Number:)
(testit "(type result017[7][0])" Number:)
(testit "(type result017[8][0])" Number:)
(testit "(type result017[9][0])" Number:)

;; For Double tests, make sure that the difference from the actual does not exceed our tolerance value
(testit "(comparefloat -9999.2345 result017[0][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat -8888.3456 result017[1][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat -7777.4567 result017[2][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat -6666.5678 result017[3][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat -5555.6789 result017[4][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 5555.6789 result017[5][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 6666.5678 result017[6][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 7777.4567 result017[7][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 8888.3456 result017[8][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 9999.2345 result017[9][0] DOUBLE_TOLERANCE)" true)

;; Results for table018
(testit "result018[\"RowCount\"]" 10)
(testit "(string result018[\"FieldList\"] true)" "#{col01: Number}")

(testit "(type result018[0][0])" Number:)
(testit "(type result018[1][0])" Number:)
(testit "(type result018[2][0])" Number:)
(testit "(type result018[3][0])" Number:)
(testit "(type result018[4][0])" Number:)
(testit "(type result018[5][0])" Number:)
(testit "(type result018[6][0])" Number:)
(testit "(type result018[7][0])" Number:)
(testit "(type result018[8][0])" Number:)
(testit "(type result018[9][0])" Number:)

;; For Double tests, make sure that the difference from the actual does not exceed our tolerance value
(testit "(comparefloat 0.000000006789 result018[0][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 0.000000005678 result018[1][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 0.000000004567 result018[2][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 0.000000003456 result018[3][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 0.000000002345 result018[4][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 55550000.06789 result018[5][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 66660000.05678 result018[6][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 77770000.04567 result018[7][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 88880000.03456 result018[8][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 99990000.02345 result018[9][0] DOUBLE_TOLERANCE)" true)

;; Results for table019
(testit "result019[\"RowCount\"]" 5)
(testit "(string result019[\"FieldList\"] true)" "#{col01: Money}")

(testit "(type result019[0][0])" Money:)
(testit "(type result019[1][0])" Money:)
(testit "(type result019[2][0])" Money:)
(testit "(type result019[3][0])" Money:)
(testit "(type result019[4][0])" Money:)

;; For Double tests, make sure that the difference from the actual does not exceed our tolerance value
(testit "(comparefloat -9999999999 result019[0][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat -1 result019[1][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 0 result019[2][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 1 result019[3][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 9999999999 result019[4][0] DOUBLE_TOLERANCE)" true)

;; Results for table020
(testit "result020[\"RowCount\"]" 5)
(testit "(string result020[\"FieldList\"] true)" "#{col01: Money}")

(testit "(type result020[0][0])" Money:)
(testit "(type result020[1][0])" Money:)
(testit "(type result020[2][0])" Money:)
(testit "(type result020[3][0])" Money:)
(testit "(type result020[4][0])" Money:)

;; For Double tests, make sure that the difference from the actual does not exceed our tolerance value
(testit "(comparefloat 0 result020[0][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 1 result020[1][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 5000000000 result020[2][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 9999999998 result020[3][0] DOUBLE_TOLERANCE)" true)
(testit "(comparefloat 9999999999 result020[4][0] DOUBLE_TOLERANCE)" true)

;; Results for table021
(testit "result021[\"RowCount\"]" 5)
(testit "(string result021[\"FieldList\"] true)" "#{col01: Money}")

(testit "(type result021[0][0])" Money:)
(testit "(type result021[1][0])" Money:)
(testit "(type result021[2][0])" Money:)
(testit "(type result021[3][0])" Money:)
(testit "(type result021[4][0])" Money:)

(setq comp1 (string (money -1E+065)))
(setq comp2 (string (money 1E+065)))
(setq comp3 (string (money -1E+035)))
(setq comp4 (string (money 1E+035)))

(testit "(string result021[0][0] true)" comp1)
(testit "(string result021[1][0] true)" "$-1.0")
(testit "(string result021[2][0] true)" "$0.0")
(testit "(string result021[3][0] true)" "$1.0")
(testit "(string result021[4][0] true)" comp2)

;; Results for table022
(testit "result022[\"RowCount\"]" 3)
(testit "(string result022[\"FieldList\"] true)" "#{col01: Money}")

(testit "(type result022[0][0])" Money:)
(testit "(type result022[1][0])" Money:)
(testit "(type result022[2][0])" Money:)

(testit "(string result022[0][0] true)" comp3)
(testit "(string result022[1][0] true)" "$0.0")
(testit "(string result022[2][0] true)" comp4)

;; Results for table023
(testit "result023[\"RowCount\"]" 5)
(testit "(string result023[\"FieldList\"] true)" "#{col01: Date}")

(testit "(type result023[0][0])" Date:)
(testit "(type result023[1][0])" Date:)
(testit "(type result023[2][0])" Date:)
(testit "(type result023[3][0])" Date:)
(testit "(type result023[4][0])" Date:)

(testit "result023[0][0]" #Jan,10,1900)
(testit "result023[1][0]" #Feb,1,1999)
(testit "result023[2][0]" #Mar,31,2000)
(testit "result023[3][0]" #Apr,21,2008)
(testit "result023[4][0]" #May,18,2009)

;; Results for table024
(testit "result024[\"RowCount\"]" 5)
(testit "(string result024[\"FieldList\"] true)" "#{col01: Date}")

(testit "(type result024[0][0])" Date:)
(testit "(type result024[1][0])" Date:)
(testit "(type result024[2][0])" Date:)
(testit "(type result024[3][0])" Date:)
(testit "(type result024[4][0])" Date:)

(testit "(comparefloat #Jan,1,1900:00:00:01 result024[0][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat #Dec,31,1999:23:59:59 result024[1][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat #Jan,1,2000:00:00:01 result024[2][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat #Dec,31,2008:23:59:59 result024[3][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat #Jan,1,2009:00:00:01 result024[4][0] FLOAT_TOLERANCE)" true)

;; Results for table025
(testit "result025[\"RowCount\"]" 5)
(testit "(string result025[\"FieldList\"] true)" "#{col01: Date}")

(testit "(type result025[0][0])" Date:)
(testit "(type result025[1][0])" Date:)
(testit "(type result025[2][0])" Date:)
(testit "(type result025[3][0])" Date:)
(testit "(type result025[4][0])" Date:)

(testit "(comparefloat #Jan,2,1970:00:00:01 result025[0][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat #Dec,31,1999:23:59:59 result025[1][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat #Jan,1,2000:00:00:01 result025[2][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat #Dec,31,2008:23:59:59 result025[3][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat #Jan,8,2038:03:14:07 result025[4][0] FLOAT_TOLERANCE)" true)

;; Results for table026
(testit "result026[\"RowCount\"]" 5)
(testit "(string result026[\"FieldList\"] true)" "#{col01: Date}")

(testit "(type result026[0][0])" Date:)
(testit "(type result026[1][0])" Date:)
(testit "(type result026[2][0])" Date:)
(testit "(type result026[3][0])" Date:)
(testit "(type result026[4][0])" Date:)

(testit "(comparefloat #Jan,1,0 result026[0][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat #Jan,1,0:00:00:01 result026[1][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat #Jan,1,0:12:00:00 result026[2][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat #Jan,1,0:23:59:58 result026[3][0] FLOAT_TOLERANCE)" true)
(testit "(comparefloat #Jan,1,0:23:59:59 result026[4][0] FLOAT_TOLERANCE)" true)

;; Results for table027
(testit "result027[\"RowCount\"]" 6)
(testit "(string result027[\"FieldList\"] true)" "#{col01: Short}")

(testit "(type result027[0][0])" Integer:)
(testit "(type result027[1][0])" Integer:)
(testit "(type result027[2][0])" Integer:)
(testit "(type result027[3][0])" Integer:)
(testit "(type result027[4][0])" Integer:)
(testit "(type result027[5][0])" Integer:)

(testit "result027[0][0]" 1901)
(testit "result027[1][0]" 1970)
(testit "result027[2][0]" 1999)
(testit "result027[3][0]" 2000)
(testit "result027[4][0]" 2069)
(testit "result027[5][0]" 2155)

;; Results for table028
(testit "result028[\"RowCount\"]" 15)
(testit "(string result028[\"FieldList\"] true)" "#{col01: Character}")

(testit "(type result028[0][0])" Character:)
(testit "(type result028[1][0])" Character:)
(testit "(type result028[2][0])" Character:)
(testit "(type result028[3][0])" Character:)
(testit "(type result028[4][0])" Character:)
(testit "(type result028[5][0])" Character:)
(testit "(type result028[6][0])" Character:)
(testit "(type result028[7][0])" Character:)
(testit "(type result028[8][0])" Character:)
(testit "(type result028[9][0])" Character:)
(testit "(type result028[10][0])" Character:)
(testit "(type result028[11][0])" Character:)
(testit "(type result028[12][0])" Character:)
(testit "(type result028[13][0])" Character:)
(testit "(type result028[14][0])" Character:)

(testit "result028[0][0]" "0")
(testit "result028[1][0]" "1")
(testit "result028[2][0]" "2")
(testit "result028[3][0]" "3")
(testit "result028[4][0]" "4")
(testit "result028[5][0]" "a")
(testit "result028[6][0]" "b")
(testit "result028[7][0]" "c")
(testit "result028[8][0]" "d")
(testit "result028[9][0]" "e")
(testit "result028[10][0]" "V")
(testit "result028[11][0]" "W")
(testit "result028[12][0]" "X")
(testit "result028[13][0]" "Y")
(testit "result028[14][0]" "Z")

;; Results for table029
(testit "result029[\"RowCount\"]" 6)
(testit "(string result029[\"FieldList\"] true)" "#{col01: Character}")

(testit "(type result029[0][0])" Substring:)
(testit "(type result029[1][0])" Substring:)
(testit "(type result029[2][0])" Substring:)
(testit "(type result029[3][0])" Substring:)
(testit "(type result029[4][0])" Substring:)
(testit "(type result029[5][0])" Substring:)

(testit "result029[0][0]" "")
(testit "result029[1][0]" " abc")
;; Trailing Spaces are removed for CHAR
(testit "result029[2][0]" " abc")
(testit "result029[3][0]" "a")
;; Trailing Spaces are removed for CHAR
(testit "result029[4][0]" "abc")
(testit "result029[5][0]" "abcdefghij")

;; Results for table030
(testit "result030[\"RowCount\"]" 6)
(testit "(string result030[\"FieldList\"] true)" "#{col01: Object}")

(testit "(type result030[0][0])" String:)
(testit "(type result030[1][0])" String:)
(testit "(type result030[2][0])" String:)
(testit "(type result030[3][0])" String:)
(testit "(type result030[4][0])" String:)
(testit "(type result030[5][0])" String:)

(testit "result030[0][0]" "")
(testit "result030[1][0]" " abc")
(testit "result030[2][0]" " abc")
(testit "result030[3][0]" "a")
(testit "result030[4][0]" "abc")
(testit "result030[5][0]" "abcdefghijklmnopqrstuvwxyz")

;; Results for table031
(testit "result031[\"RowCount\"]" 5)
(testit "(string result031[\"FieldList\"] true)" "#{col01: Character}")

(testit "(type result031[0][0])" Character:)
(testit "(type result031[1][0])" Character:)
(testit "(type result031[2][0])" Character:)
(testit "(type result031[3][0])" Character:)
(testit "(type result031[4][0])" Character:)

(testit "result031[0][0]" "a")
(testit "result031[1][0]" "b")
(testit "result031[2][0]" "c")
(testit "result031[3][0]" "d")
(testit "result031[4][0]" "e")

;; Results for table032
(testit "result032[\"RowCount\"]" 6)
(testit "(string result032[\"FieldList\"] true)" "#{col01: Object}")

(testit "(type result032[0][0])" String:)
(testit "(type result032[1][0])" String:)
(testit "(type result032[2][0])" String:)
(testit "(type result032[3][0])" String:)
(testit "(type result032[4][0])" String:)
(testit "(type result032[5][0])" String:)

(testit "result032[0][0]" "")
(testit "result032[1][0]" " abc")
;; Trailing Spaces are not removed for VARCHAR
(testit "result032[2][0]" " abc ")
(testit "result032[3][0]" "a")
;; Trailing Space are not removed for VARCHAR
(testit "result032[4][0]" "abc ")
(testit "result032[5][0]" "abcdefghijklmnopqrstuvwxyz")

;; Results for table033
(testit "result033[\"RowCount\"]" 5)
(testit "(string result033[\"FieldList\"] true)" "#{col01: Object}")

(testit "(type result033[0][0])" ByteVector:)
(testit "(type result033[1][0])" ByteVector:)
(testit "(type result033[2][0])" ByteVector:)
(testit "(type result033[3][0])" ByteVector:)
(testit "(type result033[4][0])" ByteVector:)

(testit "result033[0][0]" "a")
(testit "result033[1][0]" "b")
(testit "result033[2][0]" "c")
(testit "result033[3][0]" "d")
(testit "result033[4][0]" "e")

;; Results for table034
(testit "result034[\"RowCount\"]" 1)
(testit "(string result034[\"FieldList\"] true)" "#{col01: Object}")

(testit "(type result034[0][0])" ByteVector:)

(testit "result034[0][0]" (rept "abcde" 51))

;; Results for table035
(testit "result035[\"RowCount\"]" 5)
(testit "(string result035[\"FieldList\"] true)" "#{col01: Object}")

(testit "(type result035[0][0])" ByteVector:)
(testit "(type result035[1][0])" ByteVector:)
(testit "(type result035[2][0])" ByteVector:)
(testit "(type result035[3][0])" ByteVector:)
(testit "(type result035[4][0])" ByteVector:)

(testit "result035[0][0]" "!")
(testit "result035[1][0]" "#")
(testit "result035[2][0]" "$")
(testit "result035[3][0]" "%")
(testit "result035[4][0]" "@")

;; Results for table036
(testit "result036[\"RowCount\"]" 5)
(testit "(string result036[\"FieldList\"] true)" "#{col01: Object}")

(testit "(type result036[0][0])" ByteVector:)
(testit "(type result036[1][0])" ByteVector:)
(testit "(type result036[2][0])" ByteVector:)
(testit "(type result036[3][0])" ByteVector:)
(testit "(type result036[4][0])" ByteVector:)

(testit "result036[0][0]" "!@#$%^&*()")
(testit "result036[1][0]" "1234567890")
(testit "result036[2][0]" "asdfghjkl")
(testit "result036[3][0]" "qwertyuiop")
(testit "result036[4][0]" "zxcvbnm")

;; Results for table037
(testit "result037[\"RowCount\"]" 5)
(testit "(string result037[\"FieldList\"] true)" "#{col01: Object}")

;; In the MySQL API, TEXT types are still treated as BLOBS, the resulting AIS type will be ByteVector
(testit "(type result037[0][0])" ByteVector:)
(testit "(type result037[1][0])" ByteVector:)
(testit "(type result037[2][0])" ByteVector:)
(testit "(type result037[3][0])" ByteVector:)
(testit "(type result037[4][0])" ByteVector:)

(testit "result037[0][0]" (rept "0" 200))
(testit "result037[1][0]" (rept "1" 200))
(testit "result037[2][0]" (rept "2" 200))
(testit "result037[3][0]" (rept "3" 200))
(testit "result037[4][0]" (rept "4" 200))

;; Results for table038
(testit "result038[\"RowCount\"]" 5)
(testit "(string result038[\"FieldList\"] true)" "#{col01: Object}")

;; In the MySQL API, TEXT types are still treated as BLOBS, the resulting AIS type will be ByteVector
(testit "(type result038[0][0])" ByteVector:)
(testit "(type result038[1][0])" ByteVector:)
(testit "(type result038[2][0])" ByteVector:)
(testit "(type result038[3][0])" ByteVector:)
(testit "(type result038[4][0])" ByteVector:)

(testit "result038[0][0]" (rept "0" 400))
(testit "result038[1][0]" (rept "1" 400))
(testit "result038[2][0]" (rept "2" 400))
(testit "result038[3][0]" (rept "3" 400))
(testit "result038[4][0]" (rept "4" 400))

;; Results for table039
(testit "result039[\"RowCount\"]" 5)
(testit "(string result039[\"FieldList\"] true)" "#{col01: Object}")

;; In the MySQL API, TEXT types are still treated as BLOBS, the resulting AIS type will be ByteVector
(testit "(type result039[0][0])" ByteVector:)
(testit "(type result039[1][0])" ByteVector:)
(testit "(type result039[2][0])" ByteVector:)
(testit "(type result039[3][0])" ByteVector:)
(testit "(type result039[4][0])" ByteVector:)

(testit "result039[0][0]" (rept "0" 800))
(testit "result039[1][0]" (rept "1" 800))
(testit "result039[2][0]" (rept "2" 800))
(testit "result039[3][0]" (rept "3" 800))
(testit "result039[4][0]" (rept "4" 800))

;; Results for table040
(testit "result040[\"RowCount\"]" 5)
(testit "(string result040[\"FieldList\"] true)" "#{col01: Object}")

;; In the MySQL API, TEXT types are still treated as BLOBS, the resulting AIS type will be ByteVector
(testit "(type result040[0][0])" ByteVector:)
(testit "(type result040[1][0])" ByteVector:)
(testit "(type result040[2][0])" ByteVector:)
(testit "(type result040[3][0])" ByteVector:)
(testit "(type result040[4][0])" ByteVector:)

(testit "result040[0][0]" (rept "0" 1600))
(testit "result040[1][0]" (rept "1" 1600))
(testit "result040[2][0]" (rept "2" 1600))
(testit "result040[3][0]" (rept "3" 1600))
(testit "result040[4][0]" (rept "4" 1600))

;; Results for table041
(testit "result041[\"RowCount\"]" 5)
(testit "(string result041[\"FieldList\"] true)" "#{col01: Object}")

(testit "(type result041[0][0])" ByteVector:)
(testit "(type result041[1][0])" ByteVector:)
(testit "(type result041[2][0])" ByteVector:)
(testit "(type result041[3][0])" ByteVector:)
(testit "(type result041[4][0])" ByteVector:)

(testit "result041[0][0]" (rept "abcde" 40))
(testit "result041[1][0]" (rept "fghij" 40))
(testit "result041[2][0]" (rept "klmno" 40))
(testit "result041[3][0]" (rept "pqrst" 40))
(testit "result041[4][0]" (rept "uvwxy" 40))

;; Results for table042
(testit "result042[\"RowCount\"]" 5)
(testit "(string result042[\"FieldList\"] true)" "#{col01: Object}")

(testit "(type result042[0][0])" ByteVector:)
(testit "(type result042[1][0])" ByteVector:)
(testit "(type result042[2][0])" ByteVector:)
(testit "(type result042[3][0])" ByteVector:)
(testit "(type result042[4][0])" ByteVector:)

(testit "result042[0][0]" (rept "abcde" 80))
(testit "result042[1][0]" (rept "fghij" 80))
(testit "result042[2][0]" (rept "klmno" 80))
(testit "result042[3][0]" (rept "pqrst" 80))
(testit "result042[4][0]" (rept "uvwxy" 80))

;; Results for table043
(testit "result043[\"RowCount\"]" 5)
(testit "(string result043[\"FieldList\"] true)" "#{col01: Object}")

(testit "(type result043[0][0])" ByteVector:)
(testit "(type result043[1][0])" ByteVector:)
(testit "(type result043[2][0])" ByteVector:)
(testit "(type result043[3][0])" ByteVector:)
(testit "(type result043[4][0])" ByteVector:)

(testit "result043[0][0]" (rept "abcde" 160))
(testit "result043[1][0]" (rept "fghij" 160))
(testit "result043[2][0]" (rept "klmno" 160))
(testit "result043[3][0]" (rept "pqrst" 160))
(testit "result043[4][0]" (rept "uvwxy" 160))

;; Results for table044
(testit "result044[\"RowCount\"]" 5)
(testit "(string result044[\"FieldList\"] true)" "#{col01: Object}")

(testit "(type result044[0][0])" ByteVector:)
(testit "(type result044[1][0])" ByteVector:)
(testit "(type result044[2][0])" ByteVector:)
(testit "(type result044[3][0])" ByteVector:)
(testit "(type result044[4][0])" ByteVector:)

(testit "result044[0][0]" (rept "abcde" 320))
(testit "result044[1][0]" (rept "fghij" 320))
(testit "result044[2][0]" (rept "klmno" 320))
(testit "result044[3][0]" (rept "pqrst" 320))
(testit "result044[4][0]" (rept "uvwxy" 320))

;; End of data type handling verification tests

;; DROP
(writeln "Dropping Tables...")
(testit "(sql sqlHndl {DROP TABLE table001})" 0)
(testit "(sql sqlHndl {DROP TABLE table002})" 0)
(testit "(sql sqlHndl {DROP TABLE table003})" 0)
(testit "(sql sqlHndl {DROP TABLE table004})" 0)
(testit "(sql sqlHndl {DROP TABLE table005})" 0)
(testit "(sql sqlHndl {DROP TABLE table006})" 0)
(testit "(sql sqlHndl {DROP TABLE table007})" 0)
(testit "(sql sqlHndl {DROP TABLE table008})" 0)
(testit "(sql sqlHndl {DROP TABLE table009})" 0)
(testit "(sql sqlHndl {DROP TABLE table010})" 0)
(testit "(sql sqlHndl {DROP TABLE table011})" 0)
(testit "(sql sqlHndl {DROP TABLE table012})" 0)
(testit "(sql sqlHndl {DROP TABLE table013})" 0)
(testit "(sql sqlHndl {DROP TABLE table014})" 0)
(testit "(sql sqlHndl {DROP TABLE table015})" 0)
(testit "(sql sqlHndl {DROP TABLE table016})" 0)
(testit "(sql sqlHndl {DROP TABLE table017})" 0)
(testit "(sql sqlHndl {DROP TABLE table018})" 0)
(testit "(sql sqlHndl {DROP TABLE table019})" 0)
(testit "(sql sqlHndl {DROP TABLE table020})" 0)
(testit "(sql sqlHndl {DROP TABLE table021})" 0)
(testit "(sql sqlHndl {DROP TABLE table022})" 0)
(testit "(sql sqlHndl {DROP TABLE table023})" 0)
(testit "(sql sqlHndl {DROP TABLE table024})" 0)
(testit "(sql sqlHndl {DROP TABLE table025})" 0)
(testit "(sql sqlHndl {DROP TABLE table026})" 0)
(testit "(sql sqlHndl {DROP TABLE table027})" 0)
(testit "(sql sqlHndl {DROP TABLE table028})" 0)
(testit "(sql sqlHndl {DROP TABLE table029})" 0)
(testit "(sql sqlHndl {DROP TABLE table030})" 0)
(testit "(sql sqlHndl {DROP TABLE table031})" 0)
(testit "(sql sqlHndl {DROP TABLE table032})" 0)
(testit "(sql sqlHndl {DROP TABLE table033})" 0)
(testit "(sql sqlHndl {DROP TABLE table034})" 0)
(testit "(sql sqlHndl {DROP TABLE table035})" 0)
(testit "(sql sqlHndl {DROP TABLE table036})" 0)
(testit "(sql sqlHndl {DROP TABLE table037})" 0)
(testit "(sql sqlHndl {DROP TABLE table038})" 0)
(testit "(sql sqlHndl {DROP TABLE table039})" 0)
(testit "(sql sqlHndl {DROP TABLE table040})" 0)
(testit "(sql sqlHndl {DROP TABLE table041})" 0)
(testit "(sql sqlHndl {DROP TABLE table042})" 0)
(testit "(sql sqlHndl {DROP TABLE table043})" 0)
(testit "(sql sqlHndl {DROP TABLE table044})" 0)

;; Free Result Variables
(setq result001 #void)
(setq result002 #void)
(setq result003 #void)
(setq result004 #void)
(setq result005 #void)
(setq result006 #void)
(setq result007 #void)
(setq result008 #void)
(setq result009 #void)
(setq result010 #void)
(setq result011 #void)
(setq result012 #void)
(setq result013 #void)
(setq result014 #void)
(setq result015 #void)
(setq result016 #void)
(setq result017 #void)
(setq result018 #void)
(setq result019 #void)
(setq result020 #void)
(setq result021 #void)
(setq result022 #void)
(setq result023 #void)
(setq result024 #void)
(setq result025 #void)
(setq result026 #void)
(setq result027 #void)
(setq result028 #void)
(setq result029 #void)
(setq result030 #void)
(setq result031 #void)
(setq result032 #void)
(setq result033 #void)
(setq result034 #void)
(setq result035 #void)
(setq result036 #void)
(setq result037 #void)
(setq result038 #void)
(setq result039 #void)
(setq result040 #void)
(setq result041 #void)
(setq result042 #void)
(setq result043 #void)
(setq result044 #void)

(setq comp1 #void)
(setq comp2 #void)
(setq comp3 #void)
(setq comp4 #void)

;; Garbage Collection
(gc compact:)

;;************************************************************************
;; Multi-Column Handling Tests
;;************************************************************************
;; CREATE TABLE ...
(testit "(sql sqlHndl {CREATE TABLE table001 (Id INTEGER PRIMARY KEY, TinyInt1 TINYINT)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table002 (Id INTEGER PRIMARY KEY, TinyInt1 TINYINT, SmallInt1 SMALLINT, MediumInt1 MEDIUMINT)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table003 (Id INTEGER PRIMARY KEY, Integer1 INTEGER, Float1 FLOAT, Double1 DOUBLE)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table004 (Id INTEGER PRIMARY KEY, Character8 CHAR(8), Character16 CHAR(16), Integer1 INTEGER)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table005 (Id INTEGER PRIMARY KEY, Character8 CHAR(8), Integer1 INTEGER, Character16 CHAR(16))})" 0)
(testit "(sql sqlHndl {CREATE TABLE table006 (Id INTEGER PRIMARY KEY, Character24 CHAR(24), DateTime1 DATETIME, Decimal1 DECIMAL(65,30), Decimal2 DECIMAL(65,30))})" 0)
(testit "(sql sqlHndl {CREATE TABLE table007 (Id INTEGER PRIMARY KEY, Integer1 INTEGER, Integer2 INTEGER, Integer3 INTEGER)})" 0)
(testit "(sql sqlHndl {CREATE TABLE table008 (Id INTEGER PRIMARY KEY, Binary8 BINARY(8), Binary32 BINARY(32))})" 0)
(testit "(sql sqlHndl {CREATE TABLE table009 (Id INTEGER PRIMARY KEY, Integer1 INTEGER, Character32 CHAR(32))})" 0)
(testit "(sql sqlHndl {CREATE TABLE table010 (Id INTEGER PRIMARY KEY, Bit1 BIT(8), Bit2 BIT(32))})" 0)

;; Populate Reference Table (Random Values)
(setq numRows 10)

(setq shortCollection01 (new ShortVector: numRows 0)) ;; 8-bit
(setq shortCollection02 (new ShortVector: numRows 0)) ;; 16-bit
(setq longCollection01 (new LongVector: numRows 0))   ;; 24-bit
(setq longCollection02 (new LongVector: numRows 0))   ;; 32-bit
(setq intCollection01 (new IntVector: numRows 0))     ;; 32/64-bit
(setq intCollection02 (new IntVector: numRows 0))     ;; 32/64-bit
(setq intCollection03 (new IntVector: numRows 0))     ;; 32/64-bit

(setq floatCollection01 (new FltVector: numRows 0.0)) ;; 6 significant digits
(setq floatCollection02 (new FltVector: numRows 0.0)) ;; 6 significant digits
(setq numCollection01 (new NumVector: numRows 0.0))   ;; 9 significant digits
(setq numCollection02 (new NumVector: numRows 0.0))   ;; 9 significant digits
(setq moneyCollection01 (new Vector: numRows 0.0))    ;; 9 significant digits
(setq moneyCollection02 (new Vector: numRows 0.0))    ;; 9 significant digits

(setq strCollection01 (new Vector: numRows))          ;; 8-byte strings
(setq strCollection02 (new Vector: numRows))          ;; 16-byte strings
(setq strCollection03 (new Vector: numRows))          ;; 24-byte strings
(setq strCollection04 (new Vector: numRows))          ;; 32-byte strings
(setq escCollection01 (new Vector: numRows))          ;; 8-byte escaped strings
(setq escCollection02 (new Vector: numRows))          ;; 16-byte escaped strings
(setq escCollection03 (new Vector: numRows))          ;; 24-byte escaped strings
(setq escCollection04 (new Vector: numRows))          ;; 32-byte escaped strings

(setq dateCollection01 (new Vector: numRows))         ;; Date

(setq binCollection01 (new Vector: numRows))          ;; 8-byte binary string
(setq binCollection02 (new Vector: numRows))          ;; 32-byte binary string
(setq ebinCollection01 (new Vector: numRows))         ;; 8-byte escaped binary string
(setq ebinCollection02 (new Vector: numRows))         ;; 32-byte escaped binary string

(setq bitCollection01 (new Vector: numRows))          ;; 8-bit value
(setq bitCollection02 (new Vector: numRows))          ;; 32-bit value

(writeln "Populating Reference Tables...")

(loop for idx from 0 until numRows
	;; Populate Collections
	(setq shortCollection01[idx] (integer (random 127)))
	(setq shortCollection02[idx] (integer (random 32767)))
	(setq longCollection01[idx] (integer (random 8388607)))
	(setq longCollection02[idx] (integer (random 2147483647)))
	
	;; use 9223372036854775807 for 32-bit
	(setq intCollection01[idx] (integer (random 2147483647)))
	(setq intCollection02[idx] (integer (random 2147483647)))
	(setq intCollection03[idx] (integer (random 2147483647)))

	(setq floatCollection01[idx] (/ (integer (random 1000000)) (expt 10 (integer (random 10)))))
	(setq floatCollection02[idx] (* (integer (random 1000000)) (expt 10 (integer (random 10)))))

	(setq numCollection01[idx] (/ (integer (random 2147483647)) (expt 10 (integer (random 10)))))
	(setq numCollection02[idx] (* (integer (random 2147483647)) (expt 10 (integer (random 10)))))
	
	(setq moneyCollection01[idx] (money (/ (integer (random 2147483647)) (expt 10 (integer (random 10))))))
	(setq moneyCollection02[idx] (money (* (integer (random 2147483647)) (expt 10 (integer (random 10))))))

	(setq strCollection01[idx] (randomstring 8))
	(setq strCollection02[idx] (randomstring 16))
	(setq strCollection03[idx] (randomstring 24))
	(setq strCollection04[idx] (randomstring 32))
	
	(setq escCollection01[idx] (sql escape: sqlHndl strCollection01[idx]))
	(setq escCollection02[idx] (sql escape: sqlHndl strCollection02[idx]))
	(setq escCollection03[idx] (sql escape: sqlHndl strCollection03[idx]))
	(setq escCollection04[idx] (sql escape: sqlHndl strCollection04[idx]))
	
	;; 719527  = #Jan,1,1970
	;; + 22279 = #Dec,31,2030
	(setq dateCollection01[idx] (date (+ (random 22279) 719527))) ;; Random date between 1970/1/1 and 2030/12/31
	
	(setq binCollection01[idx] (randombinstring 8))
	(setq binCollection02[idx] (randombinstring 32))
	(setq ebinCollection01[idx] (sql escape: sqlHndl binCollection01[idx]))
	(setq ebinCollection02[idx] (sql escape: sqlHndl binCollection02[idx]))
    
    (setq bitCollection01[idx] (randombitstring 8))
    (setq bitCollection02[idx] (randombitstring 32))
) ;; end of outer loop

(writeln "Populating SQL Tables...")

(loop for idx from 0 until numRows
	(testit "(sql sqlHndl {INSERT INTO table001 VALUES(} idx {,} shortCollection01[idx] {)})" 1)
	(testit "(sql sqlHndl {INSERT INTO table002 VALUES(} idx {,} shortCollection01[idx] {,} shortCollection02[idx] {,} longCollection01[idx] {)})" 1)
	(testit "(sql sqlHndl {INSERT INTO table003 VALUES(} idx {,} longCollection02[idx] {,} floatCollection01[idx] {,} numCollection01[idx] {)})" 1)
	(testit "(sql sqlHndl {INSERT INTO table004 VALUES(} idx {,'} escCollection01[idx] {','} escCollection02[idx] {',} intCollection01[idx] {)})" 1)
	(testit "(sql sqlHndl {INSERT INTO table005 VALUES(} idx {,'} escCollection01[idx] {',} intCollection01[idx] {,'} escCollection02[idx] {')})" 1)
	(testit "(sql sqlHndl {INSERT INTO table006 VALUES(} idx {,'} escCollection03[idx] {',} dateCollection01[idx] {,} moneyCollection01[idx] {,} moneyCollection02[idx] {)})" 1)
	(testit "(sql sqlHndl {INSERT INTO table007 VALUES(} idx {,} intCollection01[idx] {,} intCollection02[idx] {,} intCollection03[idx] {)})" 1)
	(testit "(sql sqlHndl {INSERT INTO table008 VALUES(} idx {,'} ebinCollection01[idx] {','} ebinCollection02[idx] {')})" 1)
	(testit "(sql sqlHndl {INSERT INTO table009 VALUES(} idx {,} intCollection01[idx] {,'} escCollection04[idx] {')})" 1)
    (testit "(sql sqlHndl {INSERT INTO table010 VALUES(} idx {,} bitCollection01[idx] {,} bitCollection02[idx] {)})" 1)
) ;; end of outer loop

;; Load Tables
(writeln "Loading SQL Tables...")
(testit "(type (setq result001 (sql sqlHndl {SELECT * FROM table001 ORDER BY Id})))" Brick:)
(testit "(type (setq result002 (sql sqlHndl {SELECT * FROM table002 ORDER BY Id})))" Brick:)
(testit "(type (setq result003 (sql sqlHndl {SELECT * FROM table003 ORDER BY Id})))" Brick:)
(testit "(type (setq result004 (sql sqlHndl {SELECT * FROM table004 ORDER BY Id})))" Brick:)
(testit "(type (setq result005 (sql sqlHndl {SELECT * FROM table005 ORDER BY Id})))" Brick:)
(testit "(type (setq result006 (sql sqlHndl {SELECT * FROM table006 ORDER BY Id})))" Brick:)
(testit "(type (setq result007 (sql sqlHndl {SELECT * FROM table007 ORDER BY Id})))" Brick:)
(testit "(type (setq result008 (sql sqlHndl {SELECT * FROM table008 ORDER BY Id})))" Brick:)
(testit "(type (setq result009 (sql sqlHndl {SELECT * FROM table009 ORDER BY Id})))" Brick:)
(testit "(type (setq result010 (sql sqlHndl {SELECT * FROM table010 ORDER BY Id})))" Brick:)

;; Check Row Count
(writeln "Verifying Row Counts...")
(testit "result001[\"RowCount\"]" numRows)
(testit "result002[\"RowCount\"]" numRows)
(testit "result003[\"RowCount\"]" numRows)
(testit "result004[\"RowCount\"]" numRows)
(testit "result005[\"RowCount\"]" numRows)
(testit "result006[\"RowCount\"]" numRows)
(testit "result007[\"RowCount\"]" numRows)
(testit "result008[\"RowCount\"]" numRows)
(testit "result009[\"RowCount\"]" numRows)
(testit "result010[\"RowCount\"]" numRows)

;; Check Record Field Structure
(writeln "Verifying Record Structures...")
(testit "(string result001[\"FieldList\"] true)" "#{Id: Long TinyInt1: Short}")
(testit "(string result002[\"FieldList\"] true)" "#{Id: Long TinyInt1: Short SmallInt1: Short MediumInt1: Long}")
(testit "(string result003[\"FieldList\"] true)" "#{Id: Long Integer1: Long Float1: Float Double1: Number}")
(testit "(string result004[\"FieldList\"] true)" "#{Id: Long Character8: Character Character16: Character Integer1: Long}")
(testit "(string result005[\"FieldList\"] true)" "#{Id: Long Character8: Character Integer1: Long Character16: Character}")
(testit "(string result006[\"FieldList\"] true)" "#{Id: Long Character24: Character DateTime1: Date Decimal1: Money Decimal2: Money}")
(testit "(string result007[\"FieldList\"] true)" "#{Id: Long Integer1: Long Integer2: Long Integer3: Long}")
(testit "(string result008[\"FieldList\"] true)" "#{Id: Long Binary8: Object Binary32: Object}")
(testit "(string result009[\"FieldList\"] true)" "#{Id: Long Integer1: Long Character32: Character}")
(testit "(string result010[\"FieldList\"] true)" "#{Id: Long Bit1: Object Bit2: Object}")

;; Check Record Field Values
(loop for idx from 0 until numRows
	(testit "result001[idx][0]" idx)
	(testit "result001[idx][1]" shortCollection01[idx])

	(testit "result002[idx][0]" idx)
	(testit "result002[idx][1]" shortCollection01[idx])
	(testit "result002[idx][2]" shortCollection02[idx])
	(testit "result002[idx][3]" longCollection01[idx])

	(testit "result003[idx][0]" idx)
	(testit "result003[idx][1]" longCollection02[idx])
	(testit "(comparefloat result003[idx][2] floatCollection01[idx] FLOAT_TOLERANCE)" true)
	(testit "(comparefloat result003[idx][3] numCollection01[idx] DOUBLE_TOLERANCE)" true)

	(testit "result004[idx][0]" idx)
	(testit "result004[idx][1]" strCollection01[idx])
	(testit "result004[idx][2]" strCollection02[idx])
	(testit "result004[idx][3]" intCollection01[idx])

	(testit "result005[idx][0]" idx)
	(testit "result005[idx][1]" strCollection01[idx])
	(testit "result005[idx][2]" intCollection01[idx])
	(testit "result005[idx][3]" strCollection02[idx])

	(testit "result006[idx][0]" idx)
	(testit "result006[idx][1]" strCollection03[idx])
	(testit "(comparefloat result006[idx][2] dateCollection01[idx] FLOAT_TOLERANCE)" true)
	(testit "(comparefloat result006[idx][3] moneyCollection01[idx] DOUBLE_TOLERANCE)" true)
	(testit "(comparefloat result006[idx][4] moneyCollection02[idx] DOUBLE_TOLERANCE)" true)

	(testit "result007[idx][0]" idx)
	(testit "result007[idx][1]" intCollection01[idx])
	(testit "result007[idx][2]" intCollection02[idx])
	(testit "result007[idx][3]" intCollection03[idx])
	
	(testit "result008[idx][0]" idx)
	(testit "result008[idx][1]" binCollection01[idx])
	(testit "result008[idx][2]" binCollection02[idx])
    
    (testit "result009[idx][0]" idx)
    (testit "result009[idx][1]" intCollection01[idx])
    (testit "result009[idx][2]" strCollection04[idx])
    
	(testit "result010[idx][0]" idx)
	(testit "result010[idx][1]" bitCollection01[idx])
	(testit "result010[idx][2]" bitCollection02[idx])
)

(setq result001 #void)
(setq result002 #void)
(setq result003 #void)
(setq result004 #void)
(setq result005 #void)
(setq result006 #void)
(setq result007 #void)
(setq result008 #void)
(setq result009 #void)
(setq result010 #void)

(gc compact:)

;;************************************************************************
;; AIS Record To SQL Table Feature Tests
;;************************************************************************
(writeln "Dropping Tables...")
(testit "(sql sqlHndl {DROP TABLE table001})" 0)
(testit "(sql sqlHndl {DROP TABLE table002})" 0)
(testit "(sql sqlHndl {DROP TABLE table003})" 0)
(testit "(sql sqlHndl {DROP TABLE table004})" 0)
(testit "(sql sqlHndl {DROP TABLE table005})" 0)
(testit "(sql sqlHndl {DROP TABLE table006})" 0)
(testit "(sql sqlHndl {DROP TABLE table007})" 0)
(testit "(sql sqlHndl {DROP TABLE table008})" 0)
(testit "(sql sqlHndl {DROP TABLE table009})" 0)
(testit "(sql sqlHndl {DROP TABLE table010})" 0)

(setq numRows 10)

(writeln "Creating Record Objects...")
(setq record001 (new Brick: numRows Id:Long:1 Character8:Character:8))
(setq record002 (new Brick: numRows Id:Long:1 Date1:Date:1))
(setq record003 (new Brick: numRows Id:Long:1 Long1:Long:1))
(setq record004 (new Brick: numRows Id:Long:1 Integer1:Integer:1))
(setq record005 (new Brick: numRows Id:Long:1 Float1:Float:1))
(setq record006 (new Brick: numRows Id:Long:1 Money1:Money:1))
(setq record007 (new Brick: numRows Id:Long:1 Real1:Number:1))
(setq record008 (new Brick: numRows Id:Long:1 Short1:Short:1))
(setq record009 (new Brick: numRows Id:Long:1 Object1:Object:1))
(setq record010 (new Brick: numRows Id:Long:1 Word1:Word:1))
(setq record011 (new Brick: numRows Id:Long:1 Word1:Word:1))
(setq record012 (new Brick: numRows Id:Long:1 Character8:Character:8 Date1:Date:1 Character16:Character:16 Short1:Short:1 Long1:Long:1 Float1:Float:1 Real1:Number:1 Money1:Money:1))

;; Scenario: No Existing Tables
(writeln "Inserting Records to Database...")
(testit "(sql insert: sqlHndl \"table001\" record001)" true)
(testit "(sql insert: sqlHndl \"table002\" record002)" true)
(testit "(sql insert: sqlHndl \"table003\" record003)" true)
(testit "(sql insert: sqlHndl \"table004\" record004)" true)
(testit "(sql insert: sqlHndl \"table005\" record005)" true)
(testit "(sql insert: sqlHndl \"table006\" record006)" true)
(testit "(sql insert: sqlHndl \"table007\" record007)" true)
(testit "(sql insert: sqlHndl \"table008\" record008)" true)
(testit "(sql insert: sqlHndl \"table009\" record009)" true)
(testit "(sql insert: sqlHndl \"table010\" record010)" true)
(testit "(sql insert: sqlHndl \"table011\" record011 true)" true)
(testit "(sql insert: sqlHndl \"table012\" record012)" true)

;; Load Tables
(writeln "Loading SQL Tables...")
(testit "(type (setq result001 (sql sqlHndl {SELECT * FROM table001})))" Brick:)
(testit "(type (setq result002 (sql sqlHndl {SELECT * FROM table002})))" Brick:)
(testit "(type (setq result003 (sql sqlHndl {SELECT * FROM table003})))" Brick:)
(testit "(type (setq result004 (sql sqlHndl {SELECT * FROM table004})))" Brick:)
(testit "(type (setq result005 (sql sqlHndl {SELECT * FROM table005})))" Brick:)
(testit "(type (setq result006 (sql sqlHndl {SELECT * FROM table006})))" Brick:)
(testit "(type (setq result007 (sql sqlHndl {SELECT * FROM table007})))" Brick:)
(testit "(type (setq result008 (sql sqlHndl {SELECT * FROM table008})))" Brick:)
(testit "(type (setq result009 (sql sqlHndl {SELECT * FROM table009})))" Brick:)
(testit "(type (setq result010 (sql sqlHndl {SELECT * FROM table010})))" Brick:)
(testit "(type (setq result011 (sql sqlHndl {SELECT * FROM table011})))" Brick:)
(testit "(type (setq result012 (sql sqlHndl {SELECT * FROM table012})))" Brick:)

;; Check Row Count
(writeln "Verifying Row Counts...")
(testit "result001[\"RowCount\"]" numRows)
(testit "result002[\"RowCount\"]" numRows)
(testit "result003[\"RowCount\"]" numRows)
(testit "result004[\"RowCount\"]" numRows)
(testit "result005[\"RowCount\"]" numRows)
(testit "result006[\"RowCount\"]" numRows)
(testit "result007[\"RowCount\"]" numRows)
(testit "result008[\"RowCount\"]" numRows)
(testit "result009[\"RowCount\"]" numRows)
(testit "result010[\"RowCount\"]" numRows)
(testit "result011[\"RowCount\"]" numRows)
(testit "result012[\"RowCount\"]" numRows)

;; Check Record Field Structure
(writeln "Verifying Record Structures...")
(testit "(string result001[\"FieldList\"] true)" "#{Id: Long Character8: Character}")
(testit "(string result002[\"FieldList\"] true)" "#{Id: Long Date1: Date}")
(testit "(string result003[\"FieldList\"] true)" "#{Id: Long Long1: Long}")
(if (= archType "64-bit")
(testit "(string result004[\"FieldList\"] true)" "#{Id: Long Integer1: Integer}")
else
(testit "(string result004[\"FieldList\"] true)" "#{Id: Long Integer1: Long}")
)
(testit "(string result005[\"FieldList\"] true)" "#{Id: Long Float1: Float}")
(testit "(string result006[\"FieldList\"] true)" "#{Id: Long Money1: Money}")
(testit "(string result007[\"FieldList\"] true)" "#{Id: Long Real1: Number}")
(testit "(string result008[\"FieldList\"] true)" "#{Id: Long Short1: Short}")
(testit "(string result009[\"FieldList\"] true)" "#{Id: Long Object1: Object}")
(testit "(string result010[\"FieldList\"] true)" "#{Id: Long Word1: Object}")
(testit "(string result011[\"FieldList\"] true)" "#{Id: Long Word1: Object}")
(testit "(string result012[\"FieldList\"] true)" "#{Id: Long Character8: Character Date1: Date Character16: Character Short1: Short Long1: Long Float1: Float Real1: Number Money1: Money}")

(writeln "Verifying Field Values...")
(loop for idx from 0 until numRows
    (testit "result001[idx][0]" 0)
    (testit "result002[idx][0]" 0)
    (testit "result003[idx][0]" 0)
    (testit "result004[idx][0]" 0)
    (testit "result005[idx][0]" 0)
    (testit "result006[idx][0]" 0)
    (testit "result007[idx][0]" 0)
    (testit "result008[idx][0]" 0)
    (testit "result009[idx][0]" 0)
    (testit "result010[idx][0]" 0)
    (testit "result011[idx][0]" 0)
    (testit "result012[idx][0]" 0)
    
    (testit "result001[idx][1]" "")
    (testit "result002[idx][1]" #Jan,1,0)
    (testit "result003[idx][1]" 0)
    (testit "result004[idx][1]" 0)
    (testit "result005[idx][1]" 0.0)
    (testit "result006[idx][1]" $0.0)
    (testit "result007[idx][1]" 0.0)
    (testit "result008[idx][1]" 0)
    (testit "result009[idx][1]" #void)
    (testit "result010[idx][1]" #void)
    (testit "result011[idx][1]" #void)
    
    (testit "result012[idx][1]" "")
    (testit "result012[idx][2]" #Jan,1,0)
    (testit "result012[idx][3]" "")
    (testit "result012[idx][4]" 0)
    (testit "result012[idx][5]" 0)
    (testit "result012[idx][6]" 0.0)
    (testit "result012[idx][7]" 0.0)
    (testit "result012[idx][8]" $0.0)
)

(writeln "Deleting Table Rows...")
(testit "(sql sqlHndl {DELETE FROM table001})" numRows)
(testit "(sql sqlHndl {DELETE FROM table002})" numRows)
(testit "(sql sqlHndl {DELETE FROM table003})" numRows)
(testit "(sql sqlHndl {DELETE FROM table004})" numRows)
(testit "(sql sqlHndl {DELETE FROM table005})" numRows)
(testit "(sql sqlHndl {DELETE FROM table006})" numRows)
(testit "(sql sqlHndl {DELETE FROM table007})" numRows)
(testit "(sql sqlHndl {DELETE FROM table008})" numRows)
(testit "(sql sqlHndl {DELETE FROM table009})" numRows)
(testit "(sql sqlHndl {DELETE FROM table010})" numRows)
(testit "(sql sqlHndl {DELETE FROM table011})" numRows)
(testit "(sql sqlHndl {DELETE FROM table012})" numRows)

(writeln "Populating Record Values...")
(loop for idx from 0 until numRows
    (setq record001[idx][0] idx)
    (setq record002[idx][0] idx)
    (setq record003[idx][0] idx)
    (setq record004[idx][0] idx)
    (setq record005[idx][0] idx)
    (setq record006[idx][0] idx)
    (setq record007[idx][0] idx)
    (setq record008[idx][0] idx)
    (setq record009[idx][0] idx)
    (setq record010[idx][0] idx)
    (setq record011[idx][0] idx)
    (setq record012[idx][0] idx)

    (setq record001[idx][1] strCollection01[idx])
    (setq record002[idx][1] dateCollection01[idx])
    (setq record003[idx][1] longCollection01[idx])
    (setq record004[idx][1] intCollection01[idx])
    (setq record005[idx][1] floatCollection01[idx])
    (setq record006[idx][1] moneyCollection01[idx])
    (setq record007[idx][1] numCollection01[idx])
    (setq record008[idx][1] shortCollection01[idx])
    (setq record009[idx][1] bitCollection01[idx])
    (setq record010[idx][1] bitCollection02[idx])
    (setq record011[idx][1] bitCollection02[idx])
    
    (setq record012[idx][1] strCollection01[idx])
    (setq record012[idx][2] dateCollection01[idx])
    (setq record012[idx][3] strCollection02[idx])
    (setq record012[idx][4] shortCollection01[idx])
    (setq record012[idx][5] longCollection01[idx])
    (setq record012[idx][6] floatCollection01[idx])
    (setq record012[idx][7] numCollection01[idx])
    (setq record012[idx][8] moneyCollection01[idx])
)

;; Scenario: Existing Tables
(writeln "Inserting Records to Database...")
(testit "(sql insert: sqlHndl \"table001\" record001)" true)
(testit "(sql insert: sqlHndl \"table002\" record002)" true)
(testit "(sql insert: sqlHndl \"table003\" record003)" true)
(testit "(sql insert: sqlHndl \"table004\" record004)" true)
(testit "(sql insert: sqlHndl \"table005\" record005)" true)
(testit "(sql insert: sqlHndl \"table006\" record006)" true)
(testit "(sql insert: sqlHndl \"table007\" record007)" true)
(testit "(sql insert: sqlHndl \"table008\" record008)" true)
(testit "(sql insert: sqlHndl \"table009\" record009)" true)
(testit "(sql insert: sqlHndl \"table010\" record010)" true)
(testit "(sql insert: sqlHndl \"table011\" record011 true)" true)
(testit "(sql insert: sqlHndl \"table012\" record012)" true)

;; Load Tables
(writeln "Loading SQL Tables...")
(testit "(type (setq result001 (sql sqlHndl {SELECT * FROM table001 ORDER BY Id})))" Brick:)
(testit "(type (setq result002 (sql sqlHndl {SELECT * FROM table002 ORDER BY Id})))" Brick:)
(testit "(type (setq result003 (sql sqlHndl {SELECT * FROM table003 ORDER BY Id})))" Brick:)
(testit "(type (setq result004 (sql sqlHndl {SELECT * FROM table004 ORDER BY Id})))" Brick:)
(testit "(type (setq result005 (sql sqlHndl {SELECT * FROM table005 ORDER BY Id})))" Brick:)
(testit "(type (setq result006 (sql sqlHndl {SELECT * FROM table006 ORDER BY Id})))" Brick:)
(testit "(type (setq result007 (sql sqlHndl {SELECT * FROM table007 ORDER BY Id})))" Brick:)
(testit "(type (setq result008 (sql sqlHndl {SELECT * FROM table008 ORDER BY Id})))" Brick:)
(testit "(type (setq result009 (sql sqlHndl {SELECT * FROM table009 ORDER BY Id})))" Brick:)
(testit "(type (setq result010 (sql sqlHndl {SELECT * FROM table010 ORDER BY Id})))" Brick:)
(testit "(type (setq result011 (sql sqlHndl {SELECT * FROM table011 ORDER BY Id})))" Brick:)
(testit "(type (setq result012 (sql sqlHndl {SELECT * FROM table012 ORDER BY Id})))" Brick:)

;; Check Row Count
(writeln "Verifying Row Counts...")
(testit "result001[\"RowCount\"]" numRows)
(testit "result002[\"RowCount\"]" numRows)
(testit "result003[\"RowCount\"]" numRows)
(testit "result004[\"RowCount\"]" numRows)
(testit "result005[\"RowCount\"]" numRows)
(testit "result006[\"RowCount\"]" numRows)
(testit "result007[\"RowCount\"]" numRows)
(testit "result008[\"RowCount\"]" numRows)
(testit "result009[\"RowCount\"]" numRows)
(testit "result010[\"RowCount\"]" numRows)
(testit "result011[\"RowCount\"]" numRows)
(testit "result012[\"RowCount\"]" numRows)

(writeln "Verifying Field Values...")
(loop for idx from 0 until numRows
    (testit "result001[idx][1]" strCollection01[idx])
    (testit "(comparefloat result002[idx][1] dateCollection01[idx] FLOAT_TOLERANCE)" true)
    (testit "result003[idx][1]" longCollection01[idx])
    (testit "result004[idx][1]" intCollection01[idx])
    (testit "result005[idx][1]" floatCollection01[idx])
    (testit "result006[idx][1]" moneyCollection01[idx])
    (testit "result007[idx][1]" numCollection01[idx])
    (testit "result008[idx][1]" shortCollection01[idx])
    (testit "result009[idx][1]" bitCollection01[idx])
    (testit "result010[idx][1]" bitCollection02[idx])
    (testit "result011[idx][1]" bitCollection02[idx])
    
    (testit "result012[idx][1]" strCollection01[idx])
    (testit "(comparefloat result012[idx][2] dateCollection01[idx] FLOAT_TOLERANCE)" true)
    (testit "result012[idx][3]" strCollection02[idx])
    (testit "result012[idx][4]" shortCollection01[idx])
    (testit "result012[idx][5]" longCollection01[idx])
    (testit "(comparefloat result012[idx][6] floatCollection01[idx] FLOAT_TOLERANCE)" true)
    (testit "(comparefloat result012[idx][7] numCollection01[idx] DOUBLE_TOLERANCE)" true)
    (testit "(comparefloat result012[idx][8] moneyCollection01[idx] DOUBLE_TOLERANCE)" true)
)

;;************************************************************************
;; Disconnect
;;************************************************************************
(testit "(sql disconnect: sqlHndl)" true)

;;************************************************************************
;; Test Clean Up
;;************************************************************************
(setq result001 #void)
(setq result002 #void)
(setq result003 #void)
(setq result004 #void)
(setq result005 #void)
(setq result006 #void)
(setq result007 #void)
(setq result008 #void)
(setq result009 #void)
(setq result010 #void)
(setq result011 #void)
(setq result012 #void)

(setq record001 #void)
(setq record002 #void)
(setq record003 #void)
(setq record004 #void)
(setq record005 #void)
(setq record006 #void)
(setq record007 #void)
(setq record008 #void)
(setq record009 #void)
(setq record010 #void)
(setq record011 #void)
(setq record012 #void)

(setq shortCollection01 #void)
(setq shortCollection02 #void)
(setq longCollection01 #void)
(setq longCollection02 #void)
(setq intCollection01 #void)
(setq intCollection02 #void)
(setq intCollection03 #void)
(setq floatCollection01 #void)
(setq floatCollection02 #void)
(setq numCollection01 #void)
(setq numCollection02 #void)
(setq moneyCollection01 #void)
(setq moneyCollection02 #void)
(setq strCollection01 #void)
(setq strCollection02 #void)
(setq strCollection03 #void)
(setq strCollection04 #void)
(setq escCollection01 #void)
(setq escCollection02 #void)
(setq escCollection03 #void)
(setq escCollection04 #void)
(setq dateCollection01 #void)
(setq binCollection01 #void)
(setq binCollection02 #void)
(setq ebinCollection01 #void)
(setq ebinCollection02 #void)
(setq bitCollection01 #void)
(setq bitCollection02 #void)

(setq hostTxt #void)
(setq userTxt #void)
(setq passTxt #void)
(setq dbText #void)

(setq hostStr #void)
(setq userStr #void)
(setq passStr #void)
(setq dbStr #void)

(setq hostSym #void)
(setq userSym #void)
(setq passSym #void)
(setq dbSym #void)

;; Final Garbage Collection
(gc compact:)

(testEnd "Test_MySQLEmbeded.sl")

