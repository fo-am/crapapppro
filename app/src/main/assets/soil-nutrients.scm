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
  (choice 'rain-low
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
  (choice 'rain-medium
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
  (choice 'rain-high
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

;; when growing arable and previous crop is grass
(define previous-grass-soil-nitrogen-supply-tree
  (dtree 'soil
	 (list
	  (choice 'sandyshallow
		  (dtree 'previous-crop
			 (list
			  (choice 'grass-low-n 0)
			  (choice 'grass-other
				  (dtree 'recently-grown-grass
					 (list (choice '2yr 2) (choice 'default 1))))
			  (choice 'grass-high-n
				  (dtree 'recently-grown-grass
					 (list (choice '1yr 3) (choice '2yr 2) (choice '3ry 1)))))))
	  (choice 'deepclay
		  (dtree 'rainfall
			 (list
			  (choice 'rain-low
				  (dtree 'previous-crop
					 (list
					  (choice 'grass-low-n 2)
					  (choice 'grass-other
						  (dtree 'recently-grown-grass
							 (list (choice '1yr 3) (choice '2yr 3) (choice '3ry 2))))					  
					  (choice 'grass-high-n 
						  (dtree 'recently-grown-grass
							 (list (choice '1yr 5) (choice '2yr 4) (choice '3ry 3)))))))
			  (choice 'default
				  (dtree 'previous-crop
					 (list
					  (choice 'grass-low-n 1)
					  (choice 'grass-other
						  (dtree 'recently-grown-grass
							 (list (choice '1yr 3) (choice '2yr 2) (choice '3ry 1))))
			  
					  (choice 'grass-high-n 
						  (dtree 'recently-grown-grass
							 (list (choice '1yr 4) (choice '2yr 3) (choice '3ry 2))))))))))
	  (choice 'deepsilt ;; repeat of above
		  (dtree 'rainfall
			 (list
			  (choice 'rain-low
				  (dtree 'previous-crop
					 (list
					  (choice 'grass-low-n 2)
					  (choice 'grass-other
						  (dtree 'recently-grown-grass
							 (list (choice '1yr 3) (choice '2yr 3) (choice '3ry 2))))					  
					  (choice 'grass-high-n 
						  (dtree 'recently-grown-grass
							 (list (choice '1yr 5) (choice '2yr 4) (choice '3ry 3)))))))
			  (choice 'default
				  (dtree 'previous-crop
					 (list
					  (choice 'grass-low-n 1)
					  (choice 'grass-other
						  (dtree 'recently-grown-grass
							 (list (choice '1yr 3) (choice '2yr 2) (choice '3ry 1))))
			  
					  (choice 'grass-high-n 
						  (dtree 'recently-grown-grass
							 (list (choice '1yr 4) (choice '2yr 3) (choice '3ry 2))))))))))
	  ;; default
	  (choice 'mediumshallow
		  (dtree 'previous-crop
			 (list
			  (choice 'grass-low-n 1)
			  (choice 'grass-other 
				  (dtree 'recently-grown-grass
					 (list (choice '1yr 2) (choice '2yr 2) (choice '3ry 1))))
			  (choice 'grass-high-n 
				  (dtree 'recently-grown-grass
					 (list (choice '1yr 3) (choice '2yr 3) (choice '3ry 2))))))))))
 
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
   ((eqv? c 0) "0")
   ((eqv? c 1) "1")
   ((eqv? c 2) "2")
   ((eqv? c 3) "3")
   ((eqv? c 4) "4")
   ((eqv? c 5) "5")
   ((eqv? c 6) "6")
   ((eqv? c grassland-high-sns) "Grassland high SNS")
   ((eqv? c grassland-med-sns) "Grassland medium SNS")
   ((eqv? c grassland-low-sns) "Grassland low SNS")
   (else "SNS Unknown")))

