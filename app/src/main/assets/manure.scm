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


(define cattle-slurry-tree
  '(cattle 
    (quality 
     ((DM2 
       (nutrient 
	((nitrogen
	  (season		
	   ((autumn
	     (soil 
	      ((sandyshallow (crop ((normal 8) (grass-oilseed 16))))
	       (mediumheavy (crop ((normal 48) (grass-oilseed 56)))))))
	    (winter 48)
	    (summer 72)
	    (spring 56))))
	 (phosphorous 30) 
	 (potassium 220))))
      (DM6 
       (nutrient 
	((nitrogen
	  (season		
	   ((autumn
	     (soil 
	      ((sandyshallow (crop ((normal 13) (grass-oilseed 26))))
	       (mediumheavy (crop ((normal 65) (grass-oilseed 78)))))))
	    (winter 65)
	    (summer 91)
	    (spring 65))))
	 (phosphorous 60) 
	 (potassium 290))))
      (DM10
       (nutrient 
	((nitrogen
	  (season		
	   ((autumn
	     (soil 
	      ((sandyshallow (crop ((normal 18) (grass-oilseed 36))))
	       (mediumheavy (crop ((normal 72) (grass-oilseed 90)))))))
	    (winter 72)
	    (summer 90)
	    (spring 72))))
	 (phosphorous 90) 
	 (potassium 360))))))))

(define pig-slurry-tree
  '(pig
    (quality
     ((DM2
       (nutrient 
	((nitrogen
	  (season		
	   ((autumn
	     (soil 
	      ((sandyshallow (crop ((normal 15) (grass-oilseed 22.5))))
	       (mediumheavy (crop ((normal 52.5) (grass-oilseed 60)))))))
	    (winter 60)
	    (summer 82.5)
	    (spring 82.5))))
	 (phosphorous 25) 
	 (potassium 90))))
      (DM4-pig
       (nutrient 
	((nitrogen
	  (season		
	   ((autumn
	     (soil 
	      ((sandyshallow (crop ((normal 18) (grass-oilseed 27))))
	       (mediumheavy (crop ((normal 54) (grass-oilseed 63)))))))
	    (winter 63)
	    (summer 90)
	    (spring 90))))
	 (phosphorous 45) 
	 (potassium 110))))
      (DM6-pig
       (nutrient 
	((nitrogen
	  (season		
	   ((autumn
	     (soil 
	      ((sandyshallow (crop ((normal 22) (grass-oilseed 23))))
	       (mediumheavy (crop ((normal 55) (grass-oilseed 66)))))))
	    (winter 69)
	    (summer 99)
	    (spring 99))))
	 (phosphorous 65) 
	 (potassium 125))))))))

(define poultry-slurry-tree
  '(poultry
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
	 (potassium 86)))))
     ((broiler 
       (nutrient
	((nitrogen 
	  (season
	   ((autumn 
	     (soil 
	      ((sandyshallow (crop ((normal 30) (grass-oilseed 45))))
	       (mediumheavy (crop (normal 75) (grass-oilseed 90))))))
	    (winter (soil ((sandyshallow 60) (mediumheavy 75))))
	    (summer 90)
	    (spring 90))))
	 (phosphorous 150)
	 (potassium 162))))))))

(define fym-tree
  (quasiquote
   '(FYM
     (quality
      ((fym-cattle
	(nutrient 
	 ((nitrogen (unquote (fym-seasonal-nitrogen 6.0)))
	  (phosphorous 1.9) 
	  (potassium 7.2))))
       (fym-pig
	(nutrient 
	 ((nitrogen (unquote (fym-seasonal-nitrogen 7.0)))
	  (phosphorous 3.6) (potassium 7.2))))
       (fym-sheep
	(nutrient 
	 ((nitrogen (unquote (fym-seasonal-nitrogen 7.0)))
	  (phosphorous 1.9) (potassium 7.2))))
       (fym-duck
	(nutrient 
	 ((nitrogen (unquote (fym-seasonal-nitrogen 6.5)))
	  (phosphorous 3.3) (potassium 6.8))))
       (fym-horse
	(nutrient 
	 ((nitrogen (unquote (fym-seasonal-nitrogen 7.0)))
	  (phosphorous 3.0) (potassium 5.4)))))))))
  
(define compost-manure-tree
  '(compost
    (quality
     ((green (nutrient ((nitrogen 0.2) (phosphorous 3) (potassium 5.5))))
      (green-food (nutrient ((nitrogen 0.6) (phosphorous 3.8) (potassium 8.0))))))))

(define manure-tree
  (dtree 'type
	 (list cattle-slurry-tree
	       pig-slurry-tree
	       poultry-slurry-tree
	       fym-tree
	       compost-manure-tree)))
