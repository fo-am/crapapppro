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
		 (list (choice 'n-avail cattle-slurry-n-pc-tree)
		       (quote (p-total (quality ((DM2 0.6) ;; RB209 8th ed
						 (DM6 1.2) ;; RB209 8th ed
						 (DM10 1.8))))) ;; RB209 8th ed
		       ;; crop avail is 50% of total value (pp 67) 
		       (quote (p-avail (quality ((DM2 0.3) ;; RB209 8th ed
						 (DM6 0.6)  ;; RB209 8th ed 
						 (DM10 0.9))))) ;; RB209 8th ed
		       (quote (k-total (quality ((DM2 1.7) ;; 2017 update
						 (DM6 2.5) ;; 2017 update
						 (DM10 3.4))))) ;; 2017 update
		       ;; crop avail is 90% of total value (pp 67)
		       (quote (k-avail (quality ((DM2 1.5) ;; 2017 update 
						 (DM6 2.3) ;; 2017 update
						 (DM10 3.0))))))))) ;; 2017 update

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
		 (list (choice 'n-avail pig-slurry-n-pc-tree)
 		       (quote (p-total (quality ((DM2 0.8) ;; 2017 update
						 (DM4 1.5) ;; 2017 update
						 (DM6 2.2))))) ;; 2017 update
		       ;; 50% of total like cattle slurry
 		       (quote (p-avail (quality ((DM2 0.4) ;; 2017 update
						 (DM4 0.8) ;; 2017 update
						 (DM6 1.1))))) ;; 2017 update

		       (quote (k-total (quality ((DM2 1.8) ;; 2017 update 
						 (DM4 2.2) ;; 2017 update
						 (DM6 2.6))))) ;; 2017 update
		       ;; 90% of total like cattle slurry
		       (quote (k-avail (quality ((DM2 1.6) ;; 2017 update
						 (DM4 2.0) ;; 2017 update
						 (DM6 2.3))))))))) ;; 2017 update

;; N is in perecent of total
(define poultry-tree 
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
                (k-avail 7.7))))  ;; 2017 update (90%)
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
                 (k-avail 14)))) ;; 2017 update
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
            (k-avail 19)))) ;; 2017 update
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
              (k-avail 24))))))))) ;; 2017 update

(define fym-tree
  (choice 
   'fym
   (dtree 
    'quality
    (list (choice 'fym-cattle
		  (dtree 'nutrient 
			 (list (choice 'n-total 6.7) ;; RB209 8th ed: 6.0			  
			       (choice 'n-avail (fym-seasonal-nitrogen 6.7)) ;; RB209 8th ed: 6.0
			       (choice 'p-total 3.2) ;; RB209 2016 not significant change
			       (choice 'p-avail 1.9) ;; RB209 2016 not significant change
			       (choice 'k-total 9.4) ;; 2017 update
			       (choice 'k-avail 8.5)))) ;; (90% of 9.4) 2017 update
	  (choice 'fym-pig
		  (dtree 'nutrient 
			 (list (choice 'n-total 7.0)
			       (choice 'n-avail (fym-seasonal-nitrogen 7.0)) ;; RB209 2016 no recommended changes
			       (choice 'p-total 6.0)
			       (choice 'p-avail 3.6)
			       (choice 'k-total 8.0)
			       (choice 'k-avail 7.2)))) 
	  (choice 'fym-sheep
		  (dtree 'nutrient 
			 (list (choice 'n-total 7.0)
                               (choice 'n-avail (fym-seasonal-nitrogen 7.0)) ;; RB209 2016 not significant change
			       (choice 'p-total 3.2)
			       (choice 'p-avail 1.9)  ;; RB209 2016 not significant change
			       (choice 'k-total 8)
			       (choice 'k-avail 7.2))))  ;; RB209 2016 not significant change
	  (choice 'fym-duck
		  (dtree 'nutrient 
			 (list (choice 'n-total 6.5)
                               (choice 'n-avail (fym-seasonal-nitrogen 6.5))
			       (choice 'p-total 5.5) 
			       (choice 'p-avail 3.3) 
			       (choice 'k-total 7.5)
			       (choice 'k-avail 6.8))))
	  (choice 'fym-horse ;; 2016 previous RB209 values seem wrong? so leaving as is
		  (dtree 'nutrient 
			 (list (choice 'n-total 7.0)
                               (choice 'n-avail (fym-seasonal-nitrogen 7.0))
			       (choice 'p-total 5.0)
			       (choice 'p-avail 3.0)
			       (choice 'k-total 6.0)
			       (choice 'k-avail 5.4))))
	  (choice 'fym-goat ;; added from 2016 study (6 samples though)
		  (dtree 'nutrient 
			 (list (choice 'n-total 9.5)
                               (choice 'n-avail (fym-seasonal-nitrogen 9.5))
			       (choice 'p-total 4.5)
			       (choice 'p-avail 2.7) 
			       (choice 'k-total 12.2)
			       (choice 'k-avail 10.98))))))))

(define compost-manure-tree
  (quote (compost
	  (quality
	   ((green (nutrient ((n-total 0.2)
			      (n-avail 0.01) ;; (5% of total 0.2)
			      (p-total 3.4)
			      (p-avail 1.7) ;; (5%) RB209 8th ed: 3.0
			      (k-total 6.8)
			      (k-avail 5.44)))) ;; (80%) RB209 8th ed: 5.5
	    (green-food (nutrient ((n-total 0.6)
				   (n-avail 0.045) ;; (5% of total 0.6)
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

