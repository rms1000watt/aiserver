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
;Repo.sl

(defun test()
	vars:(_DBPathName m db nd i )
	(setq _DBPathName "TimsTest.db")
	(setq m 217)
	(setq db (new ObjectRepository: _DBPathName)) 
	
	(setq nd (new Directory:))
	(loop for i from 0 until m do
		(setq nd[i] (new Vector: 3 1 2 3))
	)
	(saveRepository db y: nd)
	;; (setq db.y nd)
)

(test)
