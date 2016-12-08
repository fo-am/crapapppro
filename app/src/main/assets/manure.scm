;; -*- mode: scheme; -*-
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

;; all the manure types in metric

(msg "manure.scm")

(define (pc v p) (* (/ v 100) p))

;; takes total nitrogen
(define (fym-seasonal-nitrogen n)
  (dtree 'season
	 (list 
	  (choice 'autumn
		  (dtree 'application
			 (list (choice 'straight-surface (dtree 'soil (list (choice 'sandyshallow (pc n 5)) (choice 'mediumheavy (pc n 10)))))
			       (choice 'straight-ploughed (dtree 'soil (list (choice 'sandyshallow (pc n 5)) (choice 'mediumheavy (pc n 10)))))
			       (choice 'stored-spread (dtree 'soil (list (choice 'sandyshallow (pc n 5)) (choice 'mediumheavy (pc n 10)))))
			       (choice 'stored-ploughed (dtree 'soil (list (choice 'sandyshallow (pc n 5)) (choice 'mediumheavy (pc n 10))))))))
	  (choice 'winter (dtree 'soil (list (choice 'sandyshallow (pc n 10)) (choice 'mediumheavy (pc n 10)))))
	  (choice 'spring (dtree 'application (list (choice 'straight-surface (pc n 15)) (choice 'straight-ploughed (pc n 15)) 
						    (choice 'stored-spread (pc n 10)) (choice 'stored-ploughed (pc n 10)))))
	  (choice 'summer (pc n 10)))))

;; stored separately so we can update easily
(define n-total-tree
  (quote
   (type
    ((cattle
      (quality
       ((DM2 1.6)
	(DM6 2.6)
	(DM10 3.6))))
     (pig 
      (quality
       ((DM2 3.0)
	(DM6 3.6)
	(DM10 4.4))))))))

(msg "manure.scm 1")

(define cattle-slurry-n-pc-tree
  (quote
   (application
    ((splash-surface
      (quality
       ((DM2 
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 10))))
		   (mediumheavy (crop ((normal 30) (grass-oilseed 35)))))))
	  (winter
	   (soil ((sandyshallow 30) (mediumheavy 30))))
	  (spring 45)
	  (summer 35))))
	(DM6
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 10))))
		   (mediumheavy (crop ((normal 25) (grass-oilseed 30)))))))
	   (winter
	    (soil ((sandyshallow 25) (mediumheavy 25))))
	   (spring 35) 
	   (summer 25))))
	(DM10
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 10))))
		   (mediumheavy (crop ((normal 20) (grass-oilseed 25)))))))
	   (winter
	    (soil ((sandyshallow 20) (mediumheavy 20))))
	   (spring 25)
	   (summer 20)))))))
     (splash-incorporated
      (quality
       ((DM2 
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 10))))
		   (mediumheavy (crop ((normal 35) (grass-oilseed 40)))))))
	  (winter
	    (soil ((sandyshallow 25) (mediumheavy 35))))
	  (spring 50)
	  (summer 0)))) ;; N/A
	(DM6
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 10))))
		   (mediumheavy (crop ((normal 30) (grass-oilseed 35)))))))
	   (winter
	    (soil ((sandyshallow 20) (mediumheavy 30))))
	   (spring 40) 
	   (summer 0)))) ;; N/A
	(DM10
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 10))))
		   (mediumheavy (crop ((normal 20) (grass-oilseed 25)))))))
	   (winter
	    (soil ((sandyshallow 25) (mediumheavy 25))))
	   (spring 40)
	   (summer 30)))))))
     (shoe-bar-spreader
      (quality
       ((DM2 
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 10))))
		   (mediumheavy (crop ((normal 30) (grass-oilseed 35)))))))
	  (winter
	    (soil ((sandyshallow 30) (mediumheavy 30))))
	  (spring 50)
	  (summer 40)))) 
	(DM6
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 10))))
		   (mediumheavy (crop ((normal 25) (grass-oilseed 30)))))))
	   (winter
	    (soil ((sandyshallow 25) (mediumheavy 25))))
	   (spring 40) 
	   (summer 30)))) 
	(DM10
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 10))))
		   (mediumheavy (crop ((normal 20) (grass-oilseed 25)))))))
	   (winter
	    (soil ((sandyshallow 20) (mediumheavy 20))))
	   (spring 30)
	   (summer 25)))))))
     (shallow-injected
      (quality
       ((DM2 
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 10))))
		   (mediumheavy (crop ((normal 30) (grass-oilseed 35)))))))
	  (winter
	   (soil ((sandyshallow 35) (mediumheavy 35))))
	  (spring 55)
	  (summer 45)))) 
	(DM6
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 10))))
		   (mediumheavy (crop ((normal 25) (grass-oilseed 30)))))))
	   (winter
	    (soil ((sandyshallow 30) (mediumheavy 30))))
	    (spring 45) 
	    (summer 35)))) 
	(DM10
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 10))))
		   (mediumheavy (crop ((normal 20) (grass-oilseed 25)))))))
	   (winter
	    (soil ((sandyshallow 25) (mediumheavy 25))))
	   (spring 35)
	   (summer 30)))))))))))

(msg "manure.scm 2")

;; nitrogen is % of above value
(define cattle-slurry-tree
  (choice 'cattle
	  (dtree 'nutrient
		 (list (choice 'nitrogen cattle-slurry-n-pc-tree)
		       (quote (phosphorous (quality ((DM2 0.3) (DM6 0.6) (DM10 0.9)))))
		       (quote (potassium (quality ((DM2 2.2) (DM6 2.9) (DM10 3.6)))))))))

(msg "manure.scm 3")

(define pig-slurry-n-pc-tree
  (quote
   (application
    ((splash-surface
      (quality
       ((DM2 
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		   (mediumheavy (crop ((normal 35) (grass-oilseed 40)))))))
	   (winter
	    (soil ((sandyshallow 40) (mediumheavy 40))))
	   (spring 55)
	   (summer 55))))
	(DM4
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		   (mediumheavy (crop ((normal 30) (grass-oilseed 35)))))))
	   (winter
	    (soil ((sandyshallow 35) (mediumheavy 35))))
	   (spring 50) 
	   (summer 50))))
	(DM6
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal  10) (grass-oilseed 15))))
		   (mediumheavy (crop ((normal 25) (grass-oilseed 30)))))))
	   (winter
	    (soil ((sandyshallow 30) (mediumheavy 30))))
	   (spring 45)
	   (summer 45)))))))
     (splash-incorporated
      (quality
       ((DM2 
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		   (mediumheavy (crop ((normal 45) (grass-oilseed 50)))))))
	   (winter
	    (soil ((sandyshallow 35) (mediumheavy 50))))
	   (spring 65)
	   (summer 0)))) ;; N/A
	(DM4
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		   (mediumheavy (crop ((normal 40) (grass-oilseed 45)))))))
	   (winter
	    (soil ((sandyshallow 30) (mediumheavy 45))))
	   (spring 60) 
	   (summer 0)))) ;; N/A
	(DM6
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		   (mediumheavy (crop ((normal 40) (grass-oilseed 45)))))))
	   (winter
	    (soil ((sandyshallow 25) (mediumheavy 40))))
	   (spring 55)
	   (summer 0))))))) ;; N/A
     (shoe-bar-spreader
      (quality
       ((DM2 
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		   (mediumheavy (crop ((normal 35) (grass-oilseed 40)))))))
	   (winter
	    (soil ((sandyshallow 40) (mediumheavy 40))))
	   (spring 60)
	   (summer 60)))) 
	(DM4
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		   (mediumheavy (crop ((normal 35) (grass-oilseed 40)))))))
	   (winter
	    (soil ((sandyshallow 35) (mediumheavy 35))))
	   (spring 55) 
	   (summer 55)))) 
	(DM6
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		   (mediumheavy (crop ((normal 30) (grass-oilseed 35)))))))
	   (winter
	    (soil ((sandyshallow 35) (mediumheavy 35))))
	   (spring 50)
	   (summer 50)))))))
     (shallow-injected
      (quality
       ((DM2 
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		   (mediumheavy (crop ((normal 40) (grass-oilseed 45)))))))
	   (winter
	    (soil ((sandyshallow 45) (mediumheavy 45))))
	   (spring 65)
	   (summer 65)))) 
	(DM4
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		   (mediumheavy (crop ((normal 35) (grass-oilseed 40)))))))
	   (winter
	    (soil ((sandyshallow 40) (mediumheavy 40))))
	   (spring 60) 
	   (summer 60)))) 
	(DM6
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		   (mediumheavy (crop ((normal 30) (grass-oilseed 40)))))))
	   (winter
	    (soil ((sandyshallow 40) (mediumheavy 40))))
	   (spring 55)
	   (summer 55)))))))))))

(msg "manure.scm 4")

;; nitrogen is % of total value
(define pig-slurry-tree
  (choice 'pig
	  (dtree 'nutrient
		 (list (choice 'nitrogen pig-slurry-n-pc-tree)
		       (quote (phosphorous (quality ((DM2 0.5) (DM4 0.9) (DM6 1.3)))))
		       (quote (potassium (quality ((DM2 1.8) (DM4 2.2) (DM6 2.5)))))))))

(msg "manure.scm 5")
	 	    
(define poultry-slurry-tree
  (quote (poultry
	  (quality
	   ((layer
	     (nutrient 
	      ((nitrogen
		(season		
		 ((autumn
		   (soil 
		    ((sandyshallow (crop ((normal 19) (grass-oilseed 28.5))))
		     (mediumheavy (crop ((normal 47.5) (grass-oilseed 57)))))))
		  (winter 47.5)
		  (summer 66.5)
		  (spring 66.5))))
	       (phosphorous 84) 
	       (potassium 86))))
	    (broiler 
	     (nutrient
	      ((nitrogen 
		(season
		 ((autumn 
		   (soil 
		    ((sandyshallow (crop ((normal 30) (grass-oilseed 45))))
		     (mediumheavy (crop ((normal 75) (grass-oilseed 90)))))))
		  (winter (soil ((sandyshallow 60) (mediumheavy 75))))
		  (summer 90)
		  (spring 90))))
	       (phosphorous 150)
	       (potassium 162)))))))))

(msg "manure.scm 6")

(define fym-tree
  (choice 
   'fym
   (dtree 
    'quality
    (list (choice 'fym-cattle
		  (dtree 'nutrient 
			 (list (choice 'nitrogen (fym-seasonal-nitrogen 6.0))
			       (choice 'phosphorous 1.9) 
			       (choice 'potassium 7.2))))
	  (choice 'fym-pig
		  (dtree 'nutrient 
			 (list (choice 'nitrogen (fym-seasonal-nitrogen 7.0))
			       (choice 'phosphorous 3.6) 
			       (choice 'potassium 7.2))))
	  (choice 'fym-sheep
		  (dtree 'nutrient 
			 (list (choice 'nitrogen (fym-seasonal-nitrogen 7.0))
			       (choice 'phosphorous 1.9) 
			       (choice 'potassium 7.2))))
	  (choice 'fym-duck
		  (dtree 'nutrient 
			 (list (choice 'nitrogen (fym-seasonal-nitrogen 6.5))
			       (choice 'phosphorous 3.3) 
			       (choice 'potassium 6.8))))
	  (choice 'fym-horse
		  (dtree 'nutrient 
			 (list (choice 'nitrogen (fym-seasonal-nitrogen 7.0))
			       (choice 'phosphorous 3.0) 
			       (choice 'potassium 5.4))))))))

(msg "manure.scm 7")

(define compost-manure-tree
  (quote (compost
	  (quality
	   ((green (nutrient ((nitrogen 0.2) (phosphorous 3) (potassium 5.5))))
	    (green-food (nutrient ((nitrogen 0.6) (phosphorous 3.8) (potassium 8.0)))))))))

(msg "manure.scm 8")
  

(define manure-tree
  (dtree 'type
	 (list cattle-slurry-tree
	       pig-slurry-tree
	       poultry-slurry-tree
	       fym-tree
	       compost-manure-tree)))

(msg "manure.scm end")
