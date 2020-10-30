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
;;  Title:    Multiple Constraint Shipping Script
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  Contains the LaFarge Cement Multiple Constraint sample
;;            shipping application, procedures, and Smarttables. This
;;            sample problem establishes a business model for LaFarge 
;;            Cement involving multiple Smarttables, with interlocking
;;            formulas, which define the costs for shipping the LaFarge
;;            products to its customers world wide. The objective is to
;;            minimize the shipping costs.
;;
;;  Notes:    No other dependencies.  
;;

;; *************************************************************************************
;; name:     PLANT
;; 
;; summary:  The PLANT relation decribes a shipping location, and the plant's 
;;           capacity to store products at its location.
;;            
;; fields:   Description:        The unique descriptive name of the plant.
;;           Silo_Capacity:      The local storage capacity. 
;;
;; *************************************************************************************
(define PLANT (makeSmarttable 
                  #{
                    Description: key:      
                    Silo_Capacity: formula: 
                   } 
                  PLANT:))
(define PLANT-count 20)
(loop for i from 0 until PLANT-count do
   (setq PLANT[Decription: i] (append "Plant" i))
   (setq PLANT[Silo_Capacity: i] (integer (random 100))))
     
;; *************************************************************************************
;; name:     TERMINAL
;; 
;; summary:  The TERMINAL relation decribes a shipping location, and the location's 
;;           capacity to store products.
;;            
;; fields:   Description:        The unique descriptive name of the terminal.
;;           Silo_Capacity:      The local storage capacity. 
;;
;; *************************************************************************************
(define TERMINAL (makeSmarttable 
                  #{
                    Description: key:      
                    Silo_Capacity: formula: 
                   } 
                  TERMINAL:))
(define TERMINAL-count 30)
(loop for i from 0 until TERMINAL-count do
   (setq TERMINAL[Decription: i] (append "Terminal" i))
   (setq TERMINAL[Silo_Capacity: i] (integer (random 100))))
     
;; *************************************************************************************
;; name:     PRODUCT_TYPE
;; 
;; summary:  The PRODUCT_TYPE relation decribes a product type which is
;;           produced at the plants and shipped to the customers.
;;            
;; fields:   Product_Type        The unique descriptive name of the product type.
;;           Product_Code:       The unique descriptive order code for the product.
;;           Decription:         The long description of the product. 
;;
;; *************************************************************************************
(define PRODUCT_TYPE (makeSmarttable 
                  #{
                    Product_Type: key:      
                    Product_Code: key:      
                    Description: formula: 
                   } 
                  PRODUCT_TYPE:))
(define PRODUCT_TYPE-count 15)
(loop for i from 0 until PRODUCT_TYPE-count do
   (setq PRODUCT_TYPE[Product_Type: i] (append "Product" i))
   (setq PRODUCT_TYPE[Product_Code: i] i)
   (setq PRODUCT_TYPE[Decription: i] (append "Product number " i)))

;; *************************************************************************************
;; name:     PRODUCTION_COST
;; 
;; summary:  The PRODUCTION_COST relation decribes the cost of a product type
;;           when produced at a given plant.
;;            
;; fields:   Product_Type        The unique descriptive name of the product type.
;;           Plant:              The unique descriptive name of the plant.
;;           Unit_Cost:          The cost of producing the product at this plant. 
;;
;; *************************************************************************************
(define PRODUCTION_COST (makeSmarttable 
                  #{
                    Product_Type: key:      
                    Plant: key:      
                    Unit_Cost: formula: 
                   } 
                  PRODUCTION_COST:))
(loop for i from 0 until PRODUCT_TYPE-count do
   (loop for j from 0 until PLANT-count do
      (setq PRODUCTION_COST[Product_Type: i] (append "Product" i))
      (setq PRODUCTION_COST[Plant: i] (append "Plant" j))
      (setq PRODUCTION_COST[Unit_Cost: i] (money (random 100)))))
     
;; *************************************************************************************
;; name:     SHIPPING_COST
;; 
;; summary:  The SHIPPING_COST relation decribes the cost of a product type
;;           when shipped from a given origin to a given destination by a
;;           given shipping mode.
;;            
;; fields:   Origin:             The unique descriptive name of the origin (PLANT or TERMINAL).
;;           Destination:        The unique descriptive name of the destination (PLANT or TERMINAL).
;;           Product_Type:       The unique descriptive name of the product type.
;;           Shipping_Mode:      The means of transportation (barge, rail, truck, etc.).
;;           Unit_Cost:          The cost of shipping the product. 
;;
;; *************************************************************************************
(define SHIPPING_COST (makeSmarttable 
                  #{
                    Origin: key:      
                    Destination: key:      
                    Product_Type: key:      
                    Shipping_Mode: key:      
                    Unit_Cost: formula: 
                   } 
                  SHIPPING_COST:))
(loop for i from 0 until PLANT-count do
   (loop for j from 0 until (muli TERMINAL-count .3) do
      (setq SHIPPING_COST[Origin: i] (append "Product" i))
      (setq SHIPPING_COST[Destination: j] (append "Plant" j))
      (setq SHIPPING_COST[Unit_Cost: i] (money (random 100)))))
     

;; *************************************************************************************
;; name:     FORCAST_ANNUAL_PRODUCTION
;; 
;; summary:  The FORCAST_ANNUAL_PRODUCTION relation decribes the annual production
;;           forcast for each product type at each plant.
;;            
;; fields:   Plant:              The unique descriptive name of the plant.
;;           Product_Type        The unique descriptive name of the product type.
;;           Year:               The annual period for this production forcast.
;;           Month_N_Qty:        The forcast quantity for each of the twelve months. 
;;
;; *************************************************************************************
(define FORCAST_ANNUAL_PRODUCTION (makeSmarttable 
                  #{
                    Plant: key:      
                    Product_Type: key:      
                    Year: key:      
                    Month_1_Qty: formula: 
                    Month_2_Qty: formula: 
                    Month_3_Qty: formula: 
                    Month_4_Qty: formula: 
                    Month_5_Qty: formula: 
                    Month_6_Qty: formula: 
                    Month_7_Qty: formula: 
                    Month_8_Qty: formula: 
                    Month_9_Qty: formula: 
                    Month_10_Qty: formula: 
                    Month_11_Qty: formula: 
                    Month_12_Qty: formula: 
                   } 
                  FORCAST_ANNUAL_PRODUCTION:))

;; *************************************************************************************
;; name:     FORCAST_ANNUAL_SALES
;; 
;; summary:  The FORCAST_ANNUAL_SALES relation decribes the annual sales
;;           forcast for each product type at each shipping location (PLANT or TERMINAL).
;;            
;; fields:   Plant:              The unique descriptive name of the plant.
;;           Product_Type        The unique descriptive name of the product type.
;;           Year:               The annual period for this sales forcast.
;;           Month_N_Qty:        The forcast quantity for each of the twelve months. 
;;
;; *************************************************************************************
(define FORCAST_ANNUAL_SALES (makeSmarttable 
                  #{
                    Shipping_Location: key:      
                    Product_Type: key:      
                    Year: key:      
                    Month_1_Qty: formula: 
                    Month_2_Qty: formula: 
                    Month_3_Qty: formula: 
                    Month_4_Qty: formula: 
                    Month_5_Qty: formula: 
                    Month_6_Qty: formula: 
                    Month_7_Qty: formula: 
                    Month_8_Qty: formula: 
                    Month_9_Qty: formula: 
                    Month_10_Qty: formula: 
                    Month_11_Qty: formula: 
                    Month_12_Qty: formula: 
                   } 
                  FORCAST_ANNUAL_SALES:))

;; *************************************************************************************
;; name:     CORRIDOR
;; 
;; summary:  The CORRIDOR relation decribes the shipping schedules for products from
;;           an origin shipping location (PLANT or TERMINAL) to a destination shipping
;;           location (PLANT or TERMINAL).
;;            
;; fields:   Origin:             The unique descriptive name of the origin (PLANT or TERMINAL).
;;           Destination:        The unique descriptive name of the destination (PLANT or TERMINAL).
;;           Product_Type:       The unique descriptive name of the product type.
;;           Shipping_Mode:      The means of transportation (barge, rail, truck, etc.).
;;           Year:               The annual period for this sales forcast.
;;           Month_N_Qty:        The forcast quantity for each of the twelve months. 
;;
;; *************************************************************************************
(define CORRIDOR (makeSmarttable 
                  #{
                    Origin: key:      
                    Destination: key:      
                    Product_Type: key:      
                    Shipping_Mode: key:      
                    Year: key:      
                    Month_1_Qty: formula: 
                    Month_2_Qty: formula: 
                    Month_3_Qty: formula: 
                    Month_4_Qty: formula: 
                    Month_5_Qty: formula: 
                    Month_6_Qty: formula: 
                    Month_7_Qty: formula: 
                    Month_8_Qty: formula: 
                    Month_9_Qty: formula: 
                    Month_10_Qty: formula: 
                    Month_11_Qty: formula: 
                    Month_12_Qty: formula: 
                   } 
                  CORRIDOR:))

;; *************************************************************************************
;; name:     ANNUAL_EXPENSES
;; 
;; summary:  The ANNUAL_EXPENSES relation decribes the total shipping and production costs
;;           at each location (PLANT or TERMINAL).
;;            
;; fields:   Location:           The unique descriptive name of the location (PLANT or TERMINAL).
;;           Expense_Amount:     The total shipping and production costs at the location.
;;
;; *************************************************************************************
(define ANNUAL_EXPENSES (makeSmarttable 
                  #{
                    Location: key:      
                    Expense_Amount: formula: 
                   } 
                  ANNUAL_EXPENSES:))


     




