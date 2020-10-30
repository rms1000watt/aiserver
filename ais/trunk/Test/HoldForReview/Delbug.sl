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
(defun foo(n) 
   vars:(StockST rowCount rowNum i j)
   (define _DBPathName "Test.db")
   (define db (new ObjectRepository: _DBPathName))
   (setq StockST (new Smarttable:))
   (setq StockST.name Stocks:)  

   (setq fileID (fileOpen "stocks.sbf" 0 0))
   (importTab fileID StockST )
   (fileClose fileID 1)
   (setq rowCount StockST.rowCount)
   ;;(writeln "row Count = " rowCount)
   (setq db[1] StockST)

   (loop for j from 0 until n do
       (setq StocksST db[1])

       (loop for i from 0 until 100 do
     
           (setq rowNum (integer (random rowCount)))
           ;;(writeln "deleting row " rowNum)
           (deleteRow StockST 1 rowNum)
           ) ;; end i loop

       (setq db[2] StockST)
       (setq StockST db[2])

      ) ;; end j loop
 ) ;; end defun foo






