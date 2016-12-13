;; Farm Crap App Pro Copyright (C) 2016 FoAM Kernow
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; taken from RB209 page 91-94

(define low-rainfall-sns-tree
  (choice 'low
	  (dtree 'soil
		 (list (choice 'sandyshallow 
			       (dtree 'previous-crop 
				      (list (choice 'cereals 0)
					    (choice 'low-n-veg 0)
					    (choice 'forage 0)
					    (choice 'sugarbeet 1)
					    (choice 'oilseed 1)
					    (choice 'potatoes 1)
					    (choice 'peas 1)
					    (choice 'beans 1)
					    (choice 'uncropped 1)
					    (choice 'medium-n-veg 1)
					    (choice 'high-n-veg 2))))
		       (choice 'mediumshallow
			       (dtree 'previous-crop 
				      (list (choice 'cereals 1)
					    (choice 'low-n-veg 1)
					    (choice 'forage 1)
					    (choice 'sugarbeet 2)
					    (choice 'oilseed 2)
					    (choice 'potatoes 2)
					    (choice 'peas 2)
					    (choice 'beans 2)
					    (choice 'uncropped 2)
					    (choice 'medium-n-veg 3)
					    (choice 'high-n-veg 4))))
		       (choice 'deepclay 
			       (dtree 'previous-crop 
				      (list (choice 'cereals 2)
					    (choice 'low-n-veg 2)
					    (choice 'forage 2)
					    (choice 'sugarbeet 3)
					    (choice 'oilseed 3)
					    (choice 'potatoes 3)
					    (choice 'peas 3)
					    (choice 'beans 3)
					    (choice 'uncropped 3)
					    (choice 'medium-n-veg 3)
					    (choice 'high-n-veg 4))))
		       (choice 'deepsilt 
			       (dtree 'previous-crop 
				      (list (choice 'cereals 2)
					    (choice 'low-n-veg 2)
					    (choice 'forage 2)
					    (choice 'sugarbeet 3)
					    (choice 'oilseed 3)
					    (choice 'potatoes 3)
					    (choice 'peas 3)
					    (choice 'beans 3)
					    (choice 'uncropped 3)
					    (choice 'medium-n-veg 3)
					    (choice 'high-n-veg 4))))
		       ;; midpoints
		       (choice 'organic 4)
		       (choice 'peat 5)))))

(define medium-rainfall-sns-tree
  (choice 'medium
	  (dtree 'soil
		 (list (choice 'sandyshallow 
			       (dtree 'previous-crop 
				      (list (choice 'cereals 0)
					    (choice 'low-n-veg 0)
					    (choice 'forage 0)
					    (choice 'sugarbeet 0)
					    (choice 'oilseed 0)
					    (choice 'potatoes 0)
					    (choice 'peas 1)
					    (choice 'beans 1)
					    (choice 'uncropped 1)
					    (choice 'medium-n-veg 0)
					    (choice 'high-n-veg 1))))
		       (choice 'mediumshallow
			       (dtree 'previous-crop 
				      (list (choice 'cereals 1)
					    (choice 'low-n-veg 1)
					    (choice 'forage 1)
					    (choice 'sugarbeet 1)
					    (choice 'oilseed 2)
					    (choice 'potatoes 2)
					    (choice 'peas 2)
					    (choice 'beans 2)
					    (choice 'uncropped 2)
					    (choice 'medium-n-veg 2)
					    (choice 'high-n-veg 3))))
		       (choice 'deepclay 
			       (dtree 'previous-crop 
				      (list (choice 'cereals 1)
					    (choice 'low-n-veg 1)
					    (choice 'forage 1)
					    (choice 'sugarbeet 1)
					    (choice 'oilseed 2)
					    (choice 'potatoes 2)
					    (choice 'peas 2)
					    (choice 'beans 2)
					    (choice 'uncropped 2)
					    (choice 'medium-n-veg 3)
					    (choice 'high-n-veg 4))))
		       (choice 'deepsilt 
			       (dtree 'previous-crop 
				      (list (choice 'cereals 1)
					    (choice 'low-n-veg 1)
					    (choice 'forage 1)
					    (choice 'sugarbeet 1)
					    (choice 'oilseed 2)
					    (choice 'potatoes 2)
					    (choice 'peas 2)
					    (choice 'beans 2)
					    (choice 'uncropped 2)
					    (choice 'medium-n-veg 3)
					    (choice 'high-n-veg 4))))
		       ;; midpoints
		       (choice 'organic 4)
		       (choice 'peat 5)))))

(define high-rainfall-sns-tree
  (choice 'high
	  (dtree 'soil
		 (list (choice 'sandyshallow 
			       (dtree 'previous-crop 
				      (list (choice 'cereals 0)
					    (choice 'low-n-veg 0)
					    (choice 'forage 0)
					    (choice 'sugarbeet 0)
					    (choice 'oilseed 0)
					    (choice 'potatoes 0)
					    (choice 'peas 0)
					    (choice 'beans 0)
					    (choice 'uncropped 0)
					    (choice 'medium-n-veg 0)
					    (choice 'high-n-veg 1))))
		       (choice 'mediumshallow
			       (dtree 'previous-crop 
				      (list (choice 'cereals 1)
					    (choice 'low-n-veg 1)
					    (choice 'forage 1)
					    (choice 'sugarbeet 1)
					    (choice 'oilseed 1)
					    (choice 'potatoes 1)
					    (choice 'peas 1)
					    (choice 'beans 1)
					    (choice 'uncropped 1)
					    (choice 'medium-n-veg 1)
					    (choice 'high-n-veg 2))))
		       (choice 'deepclay 
			       (dtree 'previous-crop 
				      (list (choice 'cereals 1)
					    (choice 'low-n-veg 1)
					    (choice 'forage 1)
					    (choice 'sugarbeet 1)
					    (choice 'oilseed 1)
					    (choice 'potatoes 1)
					    (choice 'peas 2)
					    (choice 'beans 2)
					    (choice 'uncropped 1)
					    (choice 'medium-n-veg 1)
					    (choice 'high-n-veg 2))))
		       (choice 'deepsilt 
			       (dtree 'previous-crop 
				      (list (choice 'cereals 1)
					    (choice 'low-n-veg 1)
					    (choice 'forage 1)
					    (choice 'sugarbeet 1)
					    (choice 'oilseed 2)
					    (choice 'potatoes 2)
					    (choice 'peas 2)
					    (choice 'beans 2)
					    (choice 'uncropped 2)
					    (choice 'medium-n-veg 2)
					    (choice 'high-n-veg 3))))
		       ;; midpoints
		       (choice 'organic 4)
		       (choice 'peat 5)))))

;; when growing arrable and previous crop is grass
(define previous-grass-soil-nitrogen-supply-tree
  (dtree 'soil
	 (list
	  (choice 'sandyshallow
		  (dtree 'previous-crop
			 (list
			  (choice 'grass-low-n 0)
			  (choice 'grass-other 1)
			  (choice 'grass-high-n 3))))
	  (choice 'deepclay
		  (dtree 'rainfall
			 (list
			  (choice 'low
				  (dtree 'previous-crop
					 (list
					  (choice 'grass-low-n 2)
					  (choice 'grass-other 3)
					  (choice 'grass-high-n 5))))
			  (choice 'default
				  (dtree 'previous-crop
					 (list
					  (choice 'grass-low-n 1)
					  (choice 'grass-other 3)
					  (choice 'grass-high-n 4)))))))
	  (choice 'deepsilt ;; repeat of above
		  (dtree 'rainfall
			 (list
			  (choice 'low
				  (dtree 'previous-crop
					 (list
					  (choice 'grass-low-n 2)
					  (choice 'grass-other 3)
					  (choice 'grass-high-n 5))))
			  (choice 'default
				  (dtree 'previous-crop
					 (list
					  (choice 'grass-low-n 1)
					  (choice 'grass-other 3)
					  (choice 'grass-high-n 4)))))))
	  ;; default
	  (choice 'mediumshallow
		  (dtree 'previous-crop
			 (list
			  (choice 'grass-low-n 1)
			  (choice 'grass-other 2)
			  (choice 'grass-high-n 3)))))))
 
(define soil-nitrogen-supply-tree
  (dtree 'rainfall
	 (list low-rainfall-sns-tree
	       medium-rainfall-sns-tree
	       high-rainfall-sns-tree)))

;; special codes for these (stored as numbers)
(define grassland-high-sns 100)
(define grassland-med-sns 101)
(define grassland-low-sns 102)

(define (soil-nutrient-code-to-text c)
  (cond
   ((eqv? c 0) "&lt;60")
   ((eqv? c 1) "61-80")
   ((eqv? c 2) "81-100")
   ((eqv? c 3) "101-120")
   ((eqv? c 4) "121-160")
   ((eqv? c 5) "161-240")
   ((eqv? c 6) "&gt;240")
   ((eqv? c grassland-high-sns) "Grassland high SNS")
   ((eqv? c grassland-med-sns) "Grassland medium SNS")
   ((eqv? c grassland-low-sns) "Grassland low SNS")
   (else "SNS Unknown")))

(define (soil-nutrient-code-to-text-imperial c)
  (cond
   ((eqv? c 0) "&lt;48")
   ((eqv? c 1) "49-64")
   ((eqv? c 2) "65-80")
   ((eqv? c 3) "81-96")
   ((eqv? c 4) "97-128")
   ((eqv? c 5) "128-192")
   ((eqv? c 6) "&gt;193")
   ((eqv? c grassland-high-sns) "Grassland high SNS")
   ((eqv? c grassland-med-sns) "Grassland medium SNS")
   ((eqv? c grassland-low-sns) "Grassland low SNS")
   (else "SNS Unknown")))

;; fixme: hmm, for csv export

(define (soil-nutrient-code-to-ascii c)
  (cond
   ((eqv? c 0) "<60")
   ((eqv? c 1) "61-80")
   ((eqv? c 2) "81-100")
   ((eqv? c 3) "101-120")
   ((eqv? c 4) "121-160")
   ((eqv? c 5) "161-240")
   ((eqv? c 6) ">240")
   ((eqv? c grassland-high-sns) "Grassland high SNS")
   ((eqv? c grassland-med-sns) "Grassland medium SNS")
   ((eqv? c grassland-low-sns) "Grassland low SNS")
   (else "SNS Unknown")))

(define (soil-nutrient-code-to-ascii-imperial c)
  (cond
   ((eqv? c 0) "<48")
   ((eqv? c 1) "49-64")
   ((eqv? c 2) "65-80")
   ((eqv? c 3) "81-96")
   ((eqv? c 4) "97-128")
   ((eqv? c 5) "128-192")
   ((eqv? c 6) ">193")
   ((eqv? c grassland-high-sns) "Grassland high SNS")
   ((eqv? c grassland-med-sns) "Grassland medium SNS")
   ((eqv? c grassland-low-sns) "Grassland low SNS")
   (else "SNS Unknown")))
