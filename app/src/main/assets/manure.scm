#lang scheme
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
	(DM4 3.6) 
	(DM6 4.7)))) 
     (poultry
      (quality
       ((DM20 9.4) ;; 2016 data no exact DM match
	(DM40 19.0)
        (DM60 28.0)
        (DM80 37.0)))))))) ;; RB209 8th ed: 30

(define cattle-slurry-n-pc-tree ; new categories added where relevant
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
	   (summer 20))))
        (dirtywater ;; 2017 update
         (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		   (mediumheavy (crop ((normal 35) (grass-oilseed 40)))))))
	   (winter
	    (soil ((sandyshallow 35) (mediumheavy 35))))
	   (spring 50)
	   (summer 30))))
        (solid ;; 2017 update
         (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 5))))
		   (mediumheavy (crop ((normal 10) (grass-oilseed 10)))))))
	   (winter
	    (soil ((sandyshallow 10) (mediumheavy 10))))
	   (spring 10)
	   (summer 10))))
        (liquidstrainer ;; used same as for DM2 2017 update
         (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 10))))
		   (mediumheavy (crop ((normal 30) (grass-oilseed 35)))))))
	  (winter
	   (soil ((sandyshallow 30) (mediumheavy 30))))
	  (spring 45)
	  (summer 35))))
        (liquidweeping ;; used same as for DM2 2017 update
         (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 10))))
		   (mediumheavy (crop ((normal 30) (grass-oilseed 35)))))))
	  (winter
	   (soil ((sandyshallow 30) (mediumheavy 30))))
	  (spring 45)
	  (summer 35))))
        (liquidmechanical ;; used same as for DM2 2017 update
         (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 10))))
		   (mediumheavy (crop ((normal 30) (grass-oilseed 35)))))))
	  (winter
	   (soil ((sandyshallow 30) (mediumheavy 30))))
	  (spring 45)
	  (summer 35)))))))
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
	   (summer 30))))
        (solid ;; 2017 update
         (season
          ((autumn
            (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 5))))
                   (mediumheavy (crop ((normal 10) (grass-oilseed 10)))))))
           (winter
            (soil ((sandyshallow 10) (mediumheavy 10))))
           (spring 10)
           (summer NA)))))))
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
(define cattle-slurry-tree ; sulphur and magnesium totals added 2017 update
  (choice 'cattle
	  (dtree 'nutrient
		 (list (choice 'n-avail cattle-slurry-n-pc-tree)
		       (quote (p-total (quality ((DM2 0.6) ;; RB209 8th ed
						 (DM6 1.2) ;; RB209 8th ed
						 (DM10 1.8) ;; RB209 8th ed
                                                 (dirtywater 0.1) ;; 2017 update
                                                 (solid 2.0) ;; 2017 update
                                                 (liquidstrainer 0.3) ;; 2017 update
                                                 (liquidweeping 0.5) ;; 2017 update
                                                 (liquidmechanical 1.2))))) ;; 2017 update
		       ;; crop avail is 50% of total value (pp 67) 
		       (quote (p-avail (quality ((DM2 0.3) ;; RB209 8th ed
						 (DM6 0.6)  ;; RB209 8th ed 
						 (DM10 0.9) ;; RB209 8th ed
                                                 (dirtywater 0.05) ;; 2017 update
                                                 (solid 1.0) ;; 2017 update
                                                 (liquidstrainer 0.15) ;; 2017 update
                                                 (liquidweeping 0.25) ;; 2017 update
                                                 (liquidmechanical 0.6))))) ;; 2017 update
		       (quote (k-total (quality ((DM2 1.7) ;; 2017 update
						 (DM6 2.5) ;; 2017 update
						 (DM10 3.4) ;; 2017 update
                                                 (dirtywater 1.0) ;; 2017 update
                                                 (solid 3.3) ;; 2017 update
                                                 (liquidstrainer 1.5) ;; 2017 update
                                                 (liquidweeping 2.3) ;; 2017 update
                                                 (liquidmechanical 2.8))))) ;; 2017 update
		       ;; crop avail is 90% of total value (pp 67)
		       (quote (k-avail (quality ((DM2 1.5) ;; 2017 update 
						 (DM6 2.3) ;; 2017 update
						 (DM10 3.0) ;; 2017 update 
                                                 (dirtywater 1.0) ;; 2017 update
                                                 (solid 3.0) ;; 2017 update
                                                 (liquidstrainer 1.4) ;; 2017 update
                                                 (liquidweeping 2.1) ;; 2017 update
                                                 (liquidmechanical 2.5))))) ;; 2017 update
                       (quote (s-total (quality ((DM2 0.3) ;; 2017 update
                                                 (DM6 0.7) ;; 2017 update
                                                 (DM10 1.0) ;; 2017 update
                                                 (dirtywater 0.1) ;; 2017 update
                                                 (solid NA) ;; 2017 update
                                                 (liquidstrainer NA) ;; 2017 update
                                                 (liquidweeping NA) ;; 2017 update
                                                 (liquidmechanical NA))))) ;; 2017 update
                       (quote (s-avail (season ((autumn
                                                 (quality ((DM2 (pc 0.3 7.5)) ;; 2017 update
                                                           (DM6 (pc 0.7 7.5)) ;; 2017 update
                                                           (DM10 (pc 1.0 7.5)) ;; 2017 update
                                                           (dirtywater (pc 0.1 7.5)) ;; 2017 update
                                                           (solid NA) ;; 2017 update
                                                           (liquidstrainer NA) ;; 2017 update
                                                           (liquidweeping NA) ;; 2017 update
                                                           (liquidmechanical NA)))) ;; 2017 update
                                                (spring
                                                 (quality ((DM2 (pc 0.3 35)) ;; 2017 update
                                                           (DM6 (pc 0.7 35)) ;; 2017 update
                                                           (DM10 (pc 1.0 35)) ;; 2017 update
                                                           (dirtywater (pc 0.1 35)) ;; 2017 update
                                                           (solid NA) ;; 2017 update
                                                           (liquidstrainer NA) ;; 2017 update
                                                           (liquidweeping NA) ;; 2017 update
                                                           (liquidmechanical NA))))
                                                (winter NA)
                                                (summer NA)))));; 2017 update
                       (quote (m-total (quality ((DM2 0.2) ;; 2017 update
                                                 (DM6 0.6) ;; 2017 update
                                                 (DM10 0.9) ;; 2017 update
                                                 (dirtywater 0.1) ;; 2017 update
                                                 (solid NA) ;; 2017 update
                                                 (liquidstrainer NA) ;; 2017 update
                                                 (liquidweeping NA) ;; 2017 update
                                                 (liquidmechanical NA))))) ;; 2017 update
                       (quote (m-avail (quality ((DM2 0.2) ;; 2017 update
                                                 (DM6 0.6) ;; 2017 update
                                                 (DM10 0.9) ;; 2017 update
                                                 (dirtywater 0.1) ;; 2017 update
                                                 (solid NA) ;; 2017 update
                                                 (liquidstrainer NA) ;; 2017 update
                                                 (liquidweeping NA) ;; 2017 update
                                                 (liquidmechanical NA))))))))) ;; 2017 update

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
	   (summer 45))))
       (liquid ;; used DM2 values 2017 update 
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 10) (grass-oilseed 15))))
		   (mediumheavy (crop ((normal 35) (grass-oilseed 40)))))))
	   (winter
	    (soil ((sandyshallow 40) (mediumheavy 40))))
	   (spring 55)
	   (summer 55))))
       (solid ;; 2017 update
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 5))))
		   (mediumheavy (crop ((normal 10) (grass-oilseed 10)))))))
	   (winter
	    (soil ((sandyshallow 10) (mediumheavy 10))))
	   (spring 10)
	   (summer 10)))))))
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
	   (summer NA)))) ;; N/A
        (solid ;; 2017 update
	 (season
	  ((autumn
	    (soil ((sandyshallow (crop ((normal 5) (grass-oilseed 5))))
		   (mediumheavy (crop ((normal 10) (grass-oilseed 10)))))))
	   (winter
	    (soil ((sandyshallow 10) (mediumheavy 10))))
	   (spring 15)
	   (summer NA))))))) 
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
(define pig-slurry-tree ;; sulphur and magnesium totals added 2017
  (choice 'pig
	  (dtree 'nutrient
		 (list (choice 'n-avail pig-slurry-n-pc-tree)
 		       (quote (p-total (quality ((DM2 0.8) ;; 2017 update
						 (DM4 1.5) ;; 2017 update
						 (DM6 2.2) ;; 2017 update
                                                 (liquid 1.1) ;; 2017 update
                                                 (solid 3.7))))) ;; 2017 update
		       ;; 50% of total like cattle slurry
 		       (quote (p-avail (quality ((DM2 0.4) ;; 2017 update
						 (DM4 0.8) ;; 2017 update
						 (DM6 1.1) ;; 2017 update
                                                 (liquid 0.6) ;; 2017 update
                                                 (solid 1.9))))) ;; 2017 update
		       (quote (k-total (quality ((DM2 1.8) ;; 2017 update 
						 (DM4 2.2) ;; 2017 update
						 (DM6 2.6) ;; 2017 update
                                                 (liquid 2.0) ;; 2017 update
                                                 (solid 2.0))))) ;; 2017 update
		       ;; 90% of total like cattle slurry
		       (quote (k-avail (quality ((DM2 1.6) ;; 2017 update
						 (DM4 2.0) ;; 2017 update
						 (DM6 2.3) ;; 2017 update
                                                 (liquid 1.8) ;; 2017 update
                                                 (solid 1.8)))) ;; 2017 update
                       (quote (s-total (quality ((DM2 0.4) ;; 2017 update
                                                 (DM4 0.7) ;; 2017 update
                                                 (DM6 1.0) ;; 2017 update
                                                 (liquid NA) ;; 2017 update
                                                 (solid NA))))) ;; 2017 update
                       (quote (s-avail (season ((autumn
                                                 (quality ((DM2 (pc 0.4 7.5)) ;; 2017 update
                                                           (DM4 (pc 0.7 7.5)) ;; 2017 update
                                                           (DM6 (pc 1.0 7.5)) ;; 2017 update
                                                           (liquid NA) ;; 2017 update
                                                           (solid NA))))) ;; 2017 update
                                               (spring
                                                (quality ((DM2 (pc 0.4 35)) ;; 2017 update
                                                           (DM4 (pc 0.7 35)) ;; 2017 update
                                                           (DM6 (pc 1.0 35)) ;; 2017 update
                                                           (liquid NA) ;; 2017 update
                                                           (solid NA)))) ;; 2017 update
                                               (summer NA) ;; 2017 update
                                               (winter NA))));; 2017 update
                       
                       (quote (m-total (quality ((DM2 0.4) ;; 2017 update
                                                 (DM4 0.7) ;; 2017 update 
                                                 (DM6 1.0) ;; 2017 update
                                                 (liquid NA) ;; 2017 update
                                                 (liquid NA)))))  ;; 2017 update
                       (quote (m-avail (quality ((DM2 0.4) ;; 2017 update
                                                 (DM4 0.7) ;; 2017 update 
                                                 (DM6 1.0) ;; 2017 update
                                                 (liquid NA) ;; 2017 update
                                                 (liquid NA))))))))))  ;; 2017 update

;; N is in perecent of total
(define poultry-tree ;; for grassland and winter oilseed cropping will need to at 5% to all Autumn values 2017 update, sulphur and magnesium totals added 2017
  (quote (poultry
          (quality
           ((DM20 ;; changed to DM instead of broiler/layer throughout 2017 update
             (nutrient 
              ((n-avail
                (application
                 ((surface
                   (season		
                    ((autumn
                      (soil 
                       ((sandyshallow (crop ((normal 15) (grass-oilseed 20)))) ;; 2017 update
                        (mediumheavy (crop ((normal 25) (grass-oilseed 30))))))) ;; 2017 update
                     (winter 25) ;; 2017 update
                     (summer 35) ;; 2017 update
                     (spring 35)))) ;; 2017 update
                 (ploughed
                  (season		
                   ((autumn
                     (soil 
                      ((sandyshallow (crop ((normal 15) (grass-oilseed 20)))) ;; 2017 update
                       (mediumheavy (crop ((normal 35) (grass-oilseed 40))))))) ;; 2017 update
                    (winter ;; 2017 update
                     (soil 
                      ((sandyshallow 25) ;; 2017 update
                       (mediumheavy 40)))) ;; 2017 update
                    (summer NA) ;; 2017 update
                    (spring 50))))))) ;; 2017 update
                (p-total 8.0) ;; 2017 update
                (p-avail 4.8)   ;; 2017 update (60%)
                (k-total 8.5) ;; 2017 update
                (k-avail 7.7) ;; 2017 update
                (s-total 3.0) ;; 2017 update
                (s-avail (season ((autumn (pc 3.0 7.5)) ;; 2017 update
                                  (spring (pc 3.0 60))))) ;; 2017 update                
                (m-total 2.7) ;; 2017 update
                (m-avail 2.7))))  ;; 2017 update (90%)
            (DM40  ;; 2017 update
             (nutrient
              ((n-avail 
                (application
                 ((surface
                   (season
                    ((autumn 
                      (soil 
                       ((sandyshallow (crop ((normal 10) (grass-oilseed 15)))) ;; 2017 update
                        (mediumheavy (crop ((normal 25) (grass-oilseed 30))))))) ;; 2017 update
                     (winter (soil ((sandyshallow 20) (mediumheavy 25)))) ;; 2017 update
                     (summer 30) ;; 2017 update
                     (spring 30)))) ;; 2017 update
                  (ploughed
                   (season		
                    ((autumn
                      (soil 
                       ((sandyshallow (crop ((normal 10) (grass-oilseed 15)))) ;; 2017 update
                        (mediumheavy (crop ((normal 30) (grass-oilseed 35))))))) ;; 2017 update
                     (winter ;; 2017 update
                      (soil 
                       ((sandyshallow 20) ;; 2017 update
                        (mediumheavy 30)))) ;; 2017 update
                     (summer NA) ;; 2017 update
                     (spring 40))))))) ;; 2017 update
                 (p-total 12) ;; 2017 update
                 (p-avail 7.2) ;; 2017 update
                 (k-total 15) ;; 2017 update
                 (k-avail 14)
                 (s-total 5.6)
                 (s-avail (season ((autumn (pc 5.6 7.5)) ;; 2017 update
                                  (spring (pc 5.6 60))))) ;; 2017 update
                 (m-total 4.3)
                 (m-avail 4.3)))) ;; 2017 update
            (DM60  ;; 2017 update
             (nutrient
              ((n-avail
                (application
                 ((surface 
                   (season
                    ((autumn 
                      (soil 
                       ((sandyshallow (crop ((normal 10) (grass-oilseed 15)))) ;; 2017 update
                        (mediumheavy (crop ((normal 25) (grass-oilseed 30))))))) ;; 2017 update
                     (winter (soil ((sandyshallow 20) (mediumheavy 25)))) ;; 2017 update
                     (summer 30) ;; 2017 update
                     (spring 30)))) ;; 2017 update
                  (ploughed
                   (season		
                    ((autumn
                      (soil 
                       ((sandyshallow (crop ((normal 10) (grass-oilseed 15)))) ;; 2017 update
                        (mediumheavy (crop ((normal 30) (grass-oilseed 35))))))) ;; 2017 update
                     (winter ;; 2017 update
                      (soil 
                       ((sandyshallow 20) ;; 2017 update
                        (mediumheavy 30)))) ;; 2017 update
                     (summer NA) ;; 2017 update
                     (spring 40))))))) ;; 2017 update
               (p-total 17) ;; 2017 update
               (p-avail 10.2) ;; 2017 update
               (k-total 21) ;; 2017 update
               (k-avail 19)
               (s-total 8.2) ;; 2017 update
               (s-avail (season ((autumn (pc 8.2 7.5)) ;; 2017 update
                                  (spring (pc 8.2 60))))) ;; 2017 update
               (m-total 5.9)
               (m-avail 5.9)))) ;; 2017 update
            (DM80  ;; 2017 update
             (nutrient
              ((n-avail 
                (application
                 ((surface 
                   (season
                    ((autumn 
                      (soil 
                       ((sandyshallow (crop ((normal 10) (grass-oilseed 15)))) ;; 2017 update
                        (mediumheavy (crop ((normal 25) (grass-oilseed 30))))))) ;; 2017 update
                     (winter (soil ((sandyshallow 20) (mediumheavy 25)))) ;; 2017 update
                     (summer 30) ;; 2017 update
                     (spring 30)))) ;; 2017 update
                  (ploughed
                   (season		
                    ((autumn
                      (soil 
                       ((sandyshallow (crop ((normal 10) (grass-oilseed 15)))) ;; 2017 update
                        (mediumheavy (crop ((normal 30) (grass-oilseed 35))))))) ;; 2017 update
                     (winter ;; 2017 update
                      (soil 
                       ((sandyshallow 20) ;; 2017 update
                        (mediumheavy 30)))) ;; 2017 update
                     (summer NA) ;; 2017 update
                     (spring 40))))))) ;; 2017 update
               (p-total 21) ;; 2017 update
               (p-avail 12.6) ;; 2017 update
               (k-total 27) ;; 2017 update
               (k-avail 24)
               (s-total 11) ;; 2017 update
               (s-avail (season ((autumn (pc 11 7.5)) ;; 2017 update
                                  (spring (pc 11 60)) ;; 2017 update
                                  (winter NA) ;; 2017 update
                                  (summer NA)))) ;; 2017 update
               (m-total 7.5) ;; 2017 update
               (m-total 7.5))))))))) ;; 2017 update

(define fym-tree ; sulphur and magnesium totals added 2017
  (choice 
   'fym
   (dtree 
    'quality
    (list (choice 'fym-cattle
		  (dtree 'nutrient 
			 (list (choice 'n-total 6.0) ;; 2017 update			  
			       (choice 'n-avail (fym-seasonal-nitrogen 6.0)) ;; 2017 update
			       (choice 'p-total 3.2) ;; RB209 2016 not significant change
			       (choice 'p-avail 1.9) ;; RB209 2016 not significant change
			       (choice 'k-total 9.4) ;; 2017 update
			       (choice 'k-avail 8.5) ;; (90% of 9.4) 2017 update
                               (choice 's-total 2.4) ;; 2017 update
                               (choice 's-avail (dtree 'season ;; 2017 update
                                                       (list (choice 'autumn (pc 2.4 7.5)) ;; 2017 update
                                                             (choice 'spring (pc 2.4 15))
                                                             (choice 'summer 'NA)
                                                             (choice 'winter 'NA)))) ;; 2017 update
                               (choice 'm-total 1.8) ;; 2017 update
                               (choice 'm-avail 1.8)))) ;; 2017 update
	  (choice 'fym-pig
		  (dtree 'nutrient 
			 (list (choice 'n-total 7.0)
			       (choice 'n-avail (fym-seasonal-nitrogen 7.0)) ;; RB209 2016 no recommended changes
			       (choice 'p-total 6.0)
			       (choice 'p-avail 3.6)
			       (choice 'k-total 8.0)
			       (choice 'k-avail 7.2)
                               (choice 's-total 3.4) ;; 2017 update
                               (choice 's-avail (dtree 'season ;; 2017 update
                                                       (list (choice 'autumn (pc 3.4 7.5)) ;; 2017 update
                                                             (choice 'spring (pc 3.4 25))
                                                             (choice 'summer 'NA)
                                                             (choice 'winter 'NA)))) ;; 2017 update
                               (choice 'm-total 1.8) ;; 2017 update
                               (choice 'm-avail 1.8)))) ;; 2017 update 
	  (choice 'fym-sheep
		  (dtree 'nutrient 
			 (list (choice 'n-total 7.0)
                               (choice 'n-avail (fym-seasonal-nitrogen 7.0)) ;; RB209 2016 not significant change
			       (choice 'p-total 3.2)
			       (choice 'p-avail 1.9)  ;; RB209 2016 not significant change
			       (choice 'k-total 8)
			       (choice 'k-avail 7.2) ;; RB209 2016 not significant change
                               (choice 's-total 4.0) ;; 2017 update
                               (choice 's-avail (dtree 'season ;; 2017 update
                                                       (list (choice 'autumn (pc 4.0 7.5)) ;; 2017 update
                                                             (choice 'spring (pc 4.0 15))
                                                             (choice 'summer 'NA)
                                                             (choice 'winter 'NA)))) ;; 2017 update
                               (choice 'm-total 2.8) ;; 2017 update
                               (choice 'm-avail 2.8)))) ;; 2017 update  
	  (choice 'fym-duck
		  (dtree 'nutrient 
			 (list (choice 'n-total 6.5)
                               (choice 'n-avail (fym-seasonal-nitrogen 6.5))
			       (choice 'p-total 5.5) 
			       (choice 'p-avail 3.3) 
			       (choice 'k-total 7.5)
			       (choice 'k-avail 6.8)
                               (choice 's-total 2.6)  ;; 2017 update
                               (choice 's-avail (dtree 'season ;; 2017 update
                                                       (list (choice 'autumn (pc 2.6 7.5)) ;; 2017 update
                                                             (choice 'spring (pc 2.6 60))
                                                             (choice 'summer 'NA)
                                                             (choice 'winter 'NA)))) ;; 2017 update
                               (choice 'm-total 2.4) ;; 2017 update
                               (choice 'm-avail 2.4)))) ;; 2017 update
	  (choice 'fym-horse ;; 2016 previous RB209 values seem wrong? so leaving as is
		  (dtree 'nutrient 
			 (list (choice 'n-total 5.0) ;;2017 update
                               (choice 'n-avail (fym-seasonal-nitrogen 5.0)) ;;2017 update
			       (choice 'p-total 5.0)
			       (choice 'p-avail 3.0)
			       (choice 'k-total 6.0)
			       (choice 'k-avail 5.4)
                               (choice 's-total 1.6)  ;; 2017 update
                               (choice 's-avail (dtree 'season ;; 2017 update
                                                       (list (choice 'autumn (pc 1.6 7.5)) ;; 2017 update
                                                             (choice 'spring (pc 1.6 15))
                                                             (choice 'summer 'NA)
                                                             (choice 'winter 'NA)))) ;; 2017 update
                               (choice 'm-total 1.5)
                               (choice 'm-avail 1.5))))
	  (choice 'fym-goat ;; added from 2016 study (6 samples though)
		  (dtree 'nutrient 
			 (list (choice 'n-total 9.5)
                               (choice 'n-avail (fym-seasonal-nitrogen 9.5))
			       (choice 'p-total 4.5)
			       (choice 'p-avail 2.7) 
			       (choice 'k-total 12.2)
			       (choice 'k-avail 10.98)
                               (choice 's-total 2.8)  ;; 2017 update
                               (choice 's-avail (dtree 'season ;; 2017 update
                                                       (list (choice 'autumn (pc 2.8 7.5)) ;; 2017 update
                                                             (choice 'spring (pc 2.8 15))
                                                             (choice 'summer 'NA)
                                                             (choice 'winter 'NA)))) ;; 2017 update
                               (choice 'm-total 1.9)
                               (choice 'm-total 1.9))))))))

(define compost-manure-tree
  (quote (compost
	  (quality
	   ((green (nutrient ((n-total 0.2)
			      (n-avail 0.01) ;; (5% of total 0.2)
			      (p-total 3.4)
			      (p-avail 1.7) ;; (5%) RB209 8th ed: 3.0
			      (k-total 6.8)
			      (k-avail 5.44) ;; (80%) RB209 8th ed: 5.5
                              (s-total 3.4)  ;; 2017 update
                              (s-avail NA) ;;no data on crop available
                              (m-total 3.4) ;; 2017 update
                              (m-avail 3.4))))  ;; 2017 update
	    (green-food (nutrient ((n-total 0.6)
				   (n-avail 0.045) ;; (5% of total 0.6)
				   (p-total 4.9)
				   (p-avail 2.45)  ;; (50%) RB209 8th ed: 3.8
				   (k-total 8.0)
				   (k-avail 6.4)
                                   (s-total 5.1)  ;; 2017 update
                                   (s-avail NA) ;;no data on crop available
                                   (m-total 3.4) ;; 2017 update
                                   (m-avail 3.4)))))))))   ;; 2017 update

(define paper-crumble-tree ;;2017 update
  (quote (paper-crumble
          (quality
           ((chemical-physical (nutrient ((n-total 2.0)
                                          (n-avail NA) ;;no data on crop available
                                          (p-total 0.4)
                                          (p-avail NA) ;;no data on crop available
                                          (k-total 0.2)
                                          (k-avail NA) ;;no data on crop available
                                          (s-total 0.6)
                                          (s-avail NA) ;;no data on crop available
                                          (m-total 1.4)
                                          (m-avail 1.4))))
            (biological (nutrient ((n-total 7.5)
                                          (n-avail NA) ;;no data on crop available
                                          (p-total 3.8)
                                          (p-avail NA) ;;no data on crop available
                                          (k-total 0.4)
                                          (k-avail NA) ;;no data on crop available
                                          (s-total 2.4)
                                          (s-avail NA) ;;no data on crop available
                                          (m-total 1.0)
                                          (m-avail 1.0)))))))))
           
(define spent-mushroom-tree ;;2017 update
  (quote (spent-mushroom (nutrient ((n-total 6.0)
                                    (n-avail NA) ;;no data on crop available
                                    (p-total 5.0)
                                    (p-avail NA) ;;no data on crop available
                                    (k-total 9.0)
                                    (k-avail NA) ;;no data on crop available
                                    (s-total NA)
                                    (s-avail NA)
                                    (m-total NA)
                                    (m-avail NA))))))

(define water-treatment-cake-tree ;;2017 update
  (quote (water-treatment-cake (nutrient ((n-total 2.4)
                                    (n-avail NA) ;;no data on crop available
                                    (p-total 3.4)
                                    (p-avail NA) ;;no data on crop available
                                    (k-total 0.4)
                                    (k-avail NA) ;;no data on crop available
                                    (s-total 5.5)
                                    (s-avail NA) ;;no data on crop available
                                    (m-total 0.8)
                                    (m-avail 0.8)))))) 

(define food-industry-waste-tree ;;2017 update
  (quote (food-industry-waste
          (quality
           ((dairy (nutrient ((n-total 1.0)
                              (n-avail NA) ;;no data on crop available
                              (p-total 0.8)
                              (p-avail NA) ;;no data on crop available
                              (k-total 0.2)
                              (k-avail NA) ;;no data on crop available
                              (s-total NA)
                              (s-avail NA)
                              (m-total NA)
                              (m-avail NA))))
            (soft-drinks (nutrient ((n-total 0.3)
                                    (n-avail NA) ;;no data on crop available
                                    (p-total 0.2)
                                    (p-avail NA) ;;no data on crop available
                                    (k-total 0)
                                    (k-avail 0) 
                                    (s-total NA)
                                    (s-avail NA)
                                    (m-total NA)
                                    (m-avail NA))))
            (brewing (nutrient ((n-total 2.0)
                                (n-avail NA) ;;no data on crop available
                                (p-total 0.8)
                                (p-avail NA) ;;no data on crop available
                                (k-total 0.2)
                                (k-avail NA) ;;no data on crop available
                                (s-total NA)
                                (s-avail NA)
                                (m-total NA)
                                (m-avail NA))))
           (general (nutrient ((n-total 1.6)
                               (n-avail NA) ;;no data on crop available
                               (p-total 0.7)
                               (p-avail NA) ;;no data on crop available
                               (k-total 0.2)
                               (k-avail NA) ;;no data on crop available
                               (s-total NA)
                               (s-avail NA) 
                               (m-total NA)
                               (m-avail NA))))))))) 

(define digestate-food-tree ;;2017 update
  (choice 'digestate-food
          (dtree 'quality
		 (list 
		  (choice 'whole 
		  	  (dtree 'nutrient 
		  		 (list (choice 'n-total 4.8)
		  		       (choice 'n-avail
		  			       (dtree 'season 
		  				      (list (choice 'spring (pc 4.8 55))
		  					    (choice 'autumn 
		  						    (dtree 'crop 
		  							   (list (choice 'normal (pc 4.8 15)) 
		  								 (choice 'grass-oilseed (pc 4.8 35))))))))
		  		       (choice 'p-total 1.1)
		  		       (choice 'p-avail (pc 1.1 60))
		  		       (choice 'k-total 2.4)
		  		       (choice 'k-avail (pc 2.4 90))
		  		       (choice 's-total 0.7)
		  		       (choice 's-avail 'NA) ;;no data on crop available
		  		       (choice 'm-total 0.2)
		  		       (choice 'm-avail 0.2))))           
		  (choice 'separated-liquor 
		  	  (dtree 'nutrient (list 
		  			    (choice 'n-total 4.5)
		  			    (choice 'n-avail
		  				    (dtree 'season (list (choice 'spring (pc 4.5 55))
		  							 (choice 'autumn (dtree 'crop 
		  										(list (choice 'normal (pc 4.5 15)) 
		  										      (choice 'grass-oilseed (pc 4.5 35))))))))
		  			    (choice 'p-total 1.0)
		  			    (choice 'p-avail (pc 1.0 60))
		  			    (choice 'k-total 2.8)
		  			    (choice 'k-avail (pc 2.8 90))
		  			    (choice 's-total 1.0)
		  			    (choice 's-avail 'NA)  ;;no data on crop available
		  			    (choice 'm-total '0.2)
		  			    (choice 'm-avail '0.2))))
		  (choice 'separated-fibre
		  	  (dtree 'nutrient (list 
		  			    (choice 'n-total 8.9)
		  			    (choice 'n-avail
		  				    (dtree 'season (list (choice 'spring (pc 8.9 15))
		  							 (choice 'autumn (pc 8.9 10)))))
		  			    (choice 'p-total 10.2)
		  			    (choice 'p-avail (pc 10.2 60))
		  			    (choice 'k-total 3.0)
		  			    (choice 'k-avail (pc 3.0 90))
		  			    (choice 's-total 4.1)
		  			    (choice 's-avail 'NA)  ;;no data on crop available
		  			    (choice 'm-total 2.2)
		  			    (choice 'm-avail 2.2))))
		  ))))

(define digestate-farm-tree ;;2017 update
  (choice 'digestate-farm
          (dtree 'quality
		 (list (choice 'whole 
			       (dtree 'nutrient (list (choice 'n-total 3.6)
						      (choice 'n-avail
							      (dtree 'season (list (choice 'spring (pc 3.6 55))
										  (choice 'autumn (dtree 'crop (list (choice 'normal (pc 3.6 15)) (choice 'grass-oilseed (pc 3.6 35))))))))
						      (choice 'p-total 1.7)
						      (choice 'p-avail (pc 1.7 60))
						      (choice 'k-total 4.4)
						      (choice 'k-avail (pc 4.4 90))
						      (choice 's-total 0.8)
						      (choice 's-avail 'NA) ;;no data on crop available
						      (choice 'm-total 0.6)
						      (choice 'm-avail 0.6))))           
		       (choice 'separated-liquor 
			       (dtree 'nutrient (list (choice 'n-total 1.9)
						     (choice 'n-avail
							     (dtree 'season (list (choice 'spring (pc 1.9 55))
										  (choice 'autumn (dtree crop (list (choice 'normal (pc 1.9 15)) (choice 'grass-oilseed (pc 1.9 35))))))))
						     (choice 'p-total 0.6)
						     (choice 'p-avail (pc 0.6 60))
						     (choice 'k-total 2.5)
						     (choice 'k-avail (pc 2.5 90))
						     (choice 's-total 0.1)
						     (choice 's-avail 'NA)  ;;no data on crop available
								       (choice 'm-total 0.4)
								       (choice 'm-avail 0.4))))
		       (choice 'separated-fibre 
			       (dtree 'nutrient (list (choice 'n-total 5.6)
						      (choice 'n-avail
							      (dtree 'season (list (choice 'spring (pc 5.6 15))
										   (choice 'autumn (pc 5.6 10)))))
						      (choice 'p-total 4.7)
						      (choice 'p-avail (pc 4.7 60))
						      (choice 'k-total 6.0)
						      (choice 'k-avail (pc 6.0 90))
						      (choice 's-total 2.1)
						      (choice 's-avail 'NA)  ;;no data on crop available
						      (choice 'm-total 1.8)
						      (choice 'm-avail 1.8))))))))               

(define biosolid-tree ;; 2017 update
  (quote (biosolid
          (quality
           ((digested-cake (nutrient ((n-total 11)
                                      (n-avail
                                       (application ((surface (season ((autumn
                                                                        (soil ((sandyshallow 10)
                                                                               (default 15))))
                                                                       (winter
                                                                        (soil ((sandyshallow 15)
                                                                               (default 15))))
                                                                       (spring 15)
                                                                       (summer 15))))
                                                     (ploughed (season ((autumn
                                                                         (soil ((sandyshallow 10)
                                                                                (default 15))))
                                                                        (winter
                                                                         (soil ((sandyshallow 15)
                                                                                (default 15))))
                                                                        (spring 20)
                                                                        (summer NA)))))))
                                      (p-total 11)
                                      (p-avail 5.5)
                                      (k-total 0.6)
                                      (k-avail 0.5)
                                      (s-total 8.2)
                                      (s-avail 
                                       (season ((autumn
                                                 (crop ((normal (pc 8.2 15))
                                                        (grass-oilseed (pc 8.2 27.5)))))
                                                (spring (pc 8.2 20))
                                                (winter NA)
                                                (summer NA))))
                                      (m-total 1.6)
                                      (m-avail 1.6))))
            (thermally-dried (nutrient ((n-total 40)
                                        (n-avail
                                         (application ((surface (season ((autumn
                                                                          ((soil (sandyshallow 10)
                                                                                 (default 15))))
                                                                         (winter
                                                                          ((soil (sandyshallow 15)
                                                                                 (default 15))))
                                                                         (spring 15)
                                                                         (summer 15))))
                                                       (ploughed ((season (autumn
                                                                           ((soil (sandyshallow 10)
                                                                                  (default 15))))
                                                                          (winter
                                                                           ((soil (sandyshallow 15)
                                                                                  (default 15))))
                                                                          (spring 20)
                                                                          (summer NA)))))))
                                        (p-total 55)
                                        (p-avail 28)
                                        (k-total 2.0)
                                        (k-avail 1.8)
                                        (s-total 23)
                                        (s-avail 
                                         (season ((autumn
                                                   (crop ((normal (pc 23 15))
                                                          (grass-oilseed (pc 23 27.5)))))
                                                  (spring (pc 23 20))
                                                  (winter NA)
                                                  (summer NA))))
                                        (m-total 6.0)
                                        (m-avail 6.0))))
            (lime-stabilised (nutrient ((n-total 8.5)
                                        (n-avail
                                         ((application (surface ((season (autumn
                                                                          ((soil (sandyshallow 10)
                                                                                 (default 15))))
                                                                         (winter
                                                                          ((soil (sandyshallow 15)
                                                                                 (default 15))))
                                                                         (spring 15)
                                                                         (summer 15))))
                                                       (ploughed ((season (autumn
                                                                           ((soil (sandyshallow 10)
                                                                                  (default 15))))
                                                                          (winter
                                                                           ((soil (sandyshallow 15)
                                                                                  (default 15))))
                                                                          (spring 20)
                                                                          (summer NA)))))))
                                        (p-total 7)
                                        (p-avail 3.5)
                                        (k-total 0.8)
                                        (k-avail 0.7)
                                        (s-total 7.4)
                                        (s-avail 
                                         (season ((autumn
                                                   (crop ((normal (pc 7.4 15))
                                                          (grass-oilseed (pc 7.4 27.5)))))
                                                  (spring (pc 7.4 20))
                                                  (winter NA)
                                                  (summer NA))))
                                        (m-total 2.4)
                                        (m-avail 2.4))))
            (composted (nutrient ((n-total 11)
                                  (n-avail
                                   ((application (surface ((season (autumn
                                                                    ((soil (sandyshallow 10)
                                                                           (default 15))))
                                                                   (winter
                                                                    ((soil (sandyshallow 15)
                                                                           (default 15))))
                                                                   (spring 15)
                                                                   (summer 15))))
                                                 (ploughed ((season (autumn
                                                                     ((soil (sandyshallow 10)
                                                                            (default 15))))
                                                                    (winter
                                                                     ((soil (sandyshallow 15)
                                                                            (default 15))))
                                                                    (spring 15)
                                                                    (summer NA)))))))
o                                  (p-total 10)
                                  (p-avail 5.0)
                                  (k-total 3.0)
                                  (k-avail 2.7)
                                  (s-total 6.1) 
                                  (s-avail 
                                   (season ((autumn
                                             (crop ((normal (pc 6.1 15))
                                                    (grass-oilseed (pc 6.1 27.5)))))
                                            (spring (pc 6.1 20))
                                            (winter NA)
                                            (summer NA))))
                                  (m-total 2.0)
                                  (m-avail 2.0)))))))))


(define manure-tree
  (dtree 'type
	 (list cattle-slurry-tree
	       pig-slurry-tree
	       poultry-tree
	       fym-tree
	       compost-manure-tree
               paper-crumble-tree
	       spent-mushroom-tree
               water-treatment-cake-tree
               food-industry-waste-tree
	       digestate-food-tree
	       digestate-farm-tree
	       biosolid-tree
	       )))

