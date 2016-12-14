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

;; stored separately so we can update this easily as RB209 is updated
(define n-total-tree
  (quote
   (type
    ((cattle ;; no 2016 overall recommendations for cattle slurry...
      (quality 
       ((DM2 1.5) ;; RB209 8th ed: 1.6
	(DM6 2.6)
	(DM10 3.6))))
     (pig ;; 2016 recommended no change
      (quality
       ((DM2 3.0) 
	(DM6 3.6) 
	(DM10 4.7)))) 
     (poultry
      (quality
       ((layer 19) ;; 2016 data no exact DM match
	(broiler 28)))))))) ;; RB209 8th ed: 30

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
	  (summer NA)))) ;; N/A
	(DM6
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 10))))
		   (mediumheavy (crop ((normal 30) (grass-oilseed 35)))))))
	   (winter
	    (soil ((sandyshallow 20) (mediumheavy 30))))
	   (spring 40) 
	   (summer NA)))) ;; N/A
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

;; nitrogen is % of above value
(define cattle-slurry-tree
  (choice 'cattle
	  (dtree 'nutrient
		 (list (choice 'nitrogen cattle-slurry-n-pc-tree)
		       (quote (p-total (quality ((DM2 0.5) 
						 (DM6 1.1) 
						 (DM10 1.6))))) 
		       ;; crop avail is 50% of total value (pp 67) 
		       (quote (p-avail (quality ((DM2 0.25) ;; RB209 8th ed: 0.3
						 (DM6 0.55)  ;; RB209 8th ed: 0.6
						 (DM10 0.8))))) ;; RB209 8th ed: 0.9 
		       (quote (k-total (quality ((DM2 2.4) 
						 (DM6 3.2) 
						 (DM10 3.6)))))
		       ;; crop avail is 90% of total value (pp 67)
		       (quote (k-avail (quality ((DM2 1.53) ;; RB209 8th ed: 2.2 
						 (DM6 2.25) ;; RB209 8th ed: 2.9
						 (DM10 3.06))))))))) ;; RB209 8th ed: 3.6

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
	   (summer NA)))) ;; N/A
	(DM4
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		   (mediumheavy (crop ((normal 40) (grass-oilseed 45)))))))
	   (winter
	    (soil ((sandyshallow 30) (mediumheavy 45))))
	   (spring 60) 
	   (summer NA)))) ;; N/A
	(DM6
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		   (mediumheavy (crop ((normal 40) (grass-oilseed 45)))))))
	   (winter
	    (soil ((sandyshallow 25) (mediumheavy 40))))
	   (spring 55)
	   (summer NA))))))) ;; N/A
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

;; nitrogen is % of total value
(define pig-slurry-tree
  (choice 'pig
	  (dtree 'nutrient
		 (list (choice 'nitrogen pig-slurry-n-pc-tree)
 		       (quote (p-total (quality ((DM2 0.8)
						 (DM4 1.5)
						 (DM6 2.2)))))
		       ;; 50% of total like cattle slurry
 		       (quote (p-avail (quality ((DM2 0.4) ;; RB209 8th ed: 0.5 
						 (DM4 0.75) ;; RB209 8th ed: 0.9
						 (DM6 1.1))))) ;; RB209 8th ed: 1.3

		       (quote (k-total (quality ((DM2 1.8) 
						 (DM4 2.2) 
						 (DM6 2.6)))))
		       ;; 90% of total like cattle slurry
		       (quote (k-avail (quality ((DM2 1.62) ;; RB209 8th ed: 1.8
						 (DM4 1.98) ;; RB209 8th ed: 2.2
						 (DM6 2.34))))))))) ;; RB209 8th ed: 2.5
;; no longer used...
(define old-poultry-tree
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

;; N is in perecent of total
(define poultry-tree 
  (quote (poultry
	  (quality
	   ((layer
	     (nutrient 
	      ((nitrogen
		(season		
		 ((autumn
		   (soil 
		    ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		     (mediumheavy (crop ((normal 25) (grass-oilseed 30)))))))
		  (winter 25)
		  (summer 35)
		  (spring 35))))
	       ;; recommend changing from broiler/layer to DM values??
	       (p-total 14) ;; 8th ed value
	       (p-avail 8.4)   ;; 2016 no exact match RB209 8th ed: 8.4 (60%)
	       (k-total 17) ;; 2016 value
	       (k-avail 8.6))))  ;; 2016 no exact match  RB209 8th ed: 8.6 (90%)
	    (broiler 
	     (nutrient
	      ((nitrogen 
		(season
		 ((autumn 
		   (soil 
		    ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		     (mediumheavy (crop ((normal 25) (grass-oilseed 30)))))))
		  (winter (soil ((sandyshallow 20) (mediumheavy 25))))
		  (summer 30)
		  (spring 30))))
	       (p-total 17) ;; 2016 value
	       (p-avail 10.2) ;; RB209 8th ed: 15.0
	       (k-total 21) ;; 2016 value
	       (k-avail 18.9))))))))) ;; RB209 8th ed: 16.2

(define fym-tree
  (choice 
   'fym
   (dtree 
    'quality
    (list (choice 'fym-cattle
		  (dtree 'nutrient 
			 (list (choice 'nitrogen (fym-seasonal-nitrogen 6.7)) ;; RB209 8th ed: 6.0
			       (choice 'p-total 3.2) ;; RB209 2016 not significant change
			       (choice 'p-avail 1.9) ;; RB209 2016 not significant change
			       (choice 'k-total 9.4) ;; 2016 value
			       (choice 'k-avail 8.46)))) ;; (90% of 9.4) RB209 8th ed: 7.2
	  (choice 'fym-pig
		  (dtree 'nutrient 
			 (list (choice 'nitrogen (fym-seasonal-nitrogen 7.0)) ;; RB209 2016 no recommended changes
			       (choice 'p-total 6.0)
			       (choice 'p-avail 3.6)
			       (choice 'k-total 8.0)
			       (choice 'k-avail 7.2)))) 
	  (choice 'fym-sheep
		  (dtree 'nutrient 
			 (list (choice 'nitrogen (fym-seasonal-nitrogen 7.0)) ;; RB209 2016 not significant change
			       (choice 'p-total 3.2)
			       (choice 'p-avail 1.9)  ;; RB209 2016 not significant change
			       (choice 'k-total 8)
			       (choice 'k-avail 7.2))))  ;; RB209 2016 not significant change
	  (choice 'fym-duck
		  (dtree 'nutrient 
			 (list (choice 'nitrogen (fym-seasonal-nitrogen 6.5))
			       (choice 'p-total 5.5) 
			       (choice 'p-avail 3.3) 
			       (choice 'k-total 7.5)
			       (choice 'k-avail 6.8))))
	  (choice 'fym-horse ;; 2016 previous RB209 values seem wrong? so leaving as is
		  (dtree 'nutrient 
			 (list (choice 'nitrogen (fym-seasonal-nitrogen 7.0))
			       (choice 'p-total 5.0)
			       (choice 'p-avail 3.0)
			       (choice 'k-total 6.0)
			       (choice 'k-avail 5.4))))
	  (choice 'fym-goat ;; added from 2016 study (6 samples though)
		  (dtree 'nutrient 
			 (list (choice 'nitrogen (fym-seasonal-nitrogen 9.5))
			       (choice 'p-total 4.5)
			       (choice 'p-avail 2.7) 
			       (choice 'k-total 12.2)
			       (choice 'k-avail 10.98))))))))

(define compost-manure-tree
  (quote (compost
	  (quality
	   ((green (nutrient ((nitrogen 0.01) ;; (5% of total 0.2)
			      (p-total 3.4)
			      (p-avail 1.7) ;; (5%) RB209 8th ed: 3.0
			      (k-total 6.8)
			      (k-avail 5.44)))) ;; (80%) RB209 8th ed: 5.5
	    (green-food (nutrient ((nitrogen 0.045) ;; (5% of total 0.6)
				   (p-total 4.9)
				   (p-avail 2.45)  ;; (50%) RB209 8th ed: 3.8
				   (k-total 8.0)
				   (k-avail 6.4)))))))))  ;; (80%)

(define manure-tree
  (dtree 'type
	 (list cattle-slurry-tree
	       pig-slurry-tree
	       poultry-tree
	       fym-tree
	       compost-manure-tree)))

