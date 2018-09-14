#lang scheme


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

;; taken from RB209 page 105-196
;; lots of midpoints used at high SNS indices here...

(define wheat-nitrogen-tree-feed
  '(soil
    ((sandyshallow ;; light sand   
      (sns ((0 180) (1 150) (2 120) (3  90) (4  60) (5  30) (6  20)))) ;;2017 update
     (mediumshallow ;; shallow soils
      (sns ((0 280) (1 240) (2 210) (3 180) (4 140) (5  80) (6  20))))
     (medium
      (sns ((0 250) (1 220) (2 190) (3 160) (4 120) (5  60) (6  20))))
     (deepclay
      (sns ((0 250) (1 220) (2 190) (3 160) (4 120) (5  60) (6  20))))
     (deepsilt
      (sns ((0 240) (1 210) (2 170) (3 130) (4 100) (5  40) (6  20)))) ;; 2017 update
     (organic
      (sns ((0   0) (1   0) (2   0) (3 120) (4  80) (5  60) (6  20))))
     (peat
      (sns ((0   0) (1   0) (2   0) (3   0) (4   0) (5  20) (6  20))))))) ;; 2017 update

(define wheat-nitrogen-tree-mill ;;2017 update just adds 40kg N/ha to feed table above
  '(soil
    ((sandyshallow ;; light sand   
      (sns ((0 220) (1 190) (2 160) (3  130) (4  100) (5  70) (6  60)))) ;;2017 update
     (mediumshallow ;; shallow soils
      (sns ((0 320) (1 280) (2 250) (3 220) (4 180) (5  120) (6  60)))) ;;2017 update
     (medium
      (sns ((0 290) (1 260) (2 230) (3 200) (4 160) (5  100) (6  60)))) ;;2017 update
     (deepclay
      (sns ((0 290) (1 260) (2 230) (3 190) (4 160) (5  100) (6  60)))) ;;2017 update
     (deepsilt
      (sns ((0 280) (1 250) (2 210) (3 170) (4 140) (5  80) (6  60)))) ;; 2017 update
     (organic
      (sns ((0   40) (1   40) (2   40) (3 160) (4  120) (5  100) (6  60)))) ;;2017 update
     (peat
      (sns ((0   40) (1   40) (2   40) (3   40) (4   40) (5  60) (6  60))))))) ;; 2017 update

(define barley-nitrogen-tree-feed
  '(soil
    ((sandyshallow ;; light sand
      (sns ((0 140) (1  110) (2  70) (3  50) (4  20) (5   0) (6   0)))) ;; 2017 update
     (organic
      (sns ((0   0) (1   0) (2   0) (3  70) (4  30) (5  15) (6   0))))
     (peat
      (sns ((0   0) (1   0) (2   0) (3   0) (4   0) (5  15) (6  15))))
     (default ;; other mineral soils
       (sns ((0 160) (1 140) (2 110) (3  70) (4  30) (5  15) (6   0))))
     )))

(define barley-nitrogen-tree-malt ;;2017 update
  '(soil
    ((sandyshallow ;; light sand
      (sns ((0 110) (1  80) (2  40) (3  20) (4  0) (5   0) (6   0)))) ;; 2017 update
     (organic
      (sns ((0   0) (1   0) (2   0) (3  40) (4  15) (5  0) (6   0)))) ;;2017 update
     (peat
      (sns ((0   0) (1   0) (2   0) (3   0) (4   0) (5  0) (6  0)))) ;;2017 update
     (default ;; other mineral soils
       (sns ((0 130) (1 110) (2 70) (3  40) (4  15) (5  0) (6   0)))) ;;2017 update
     )))

(define grass-nitrogen-tree ;;2017 update
  '(subtype
    ((silage 
      (targetyield
       ((DM5-7
         (cut ((1 70) (2 NA) (3 NA) (4 NA))))
        (DM7-9
         (cut ((1 80) (2 50) (3 NA) (4 NA))))
        (DM9-12
         (cut ((1 100) (2 75) (3 75) (4 NA))))  
        (DM12-15+
         (cut ((1 120) (2 90) (3 70) (4 30))))
        ))))
    (grazed
     (targetyield
      ((DM4-5
	(month ((mar 30) (default NA))))
       (DM5-7
	(month ((mar 30) (may 20) (default NA))))
       (DM6-8
	(month ((mar 30) (may 30) (jul 20) (default NA))))
       (DM7-9
	(month ((mar 40) (may 30) (jun 30) (jul 30) (default NA))))
       (DM9-12
	(month ((mar 30) (apr 30) (may 30) (jun 30) (jul 30) (aug 30) (default NA))))
       (DM10-13
	(month ((jan 30) (feb 30) (mar 40) (apr 40) (may 30) (jun 30) (jul 30) (aug 30) (default NA))))
       (DM12-15+
	(month ((jan 30) (feb 30) (mar 40) (apr 50) (may 50) (jun 40) (jul 30) (aug 30) (default NA))))
       )))
    (hay
     (sns ((grassland-low-sns 100) (grassland-med-sns 70) (grassland-high-sns 40)))
     )
    (established
     (sown
      ((spring
	(sns ((grassland-low-sns 60) (grassland-med-sns 60) (grassland-high-sns 60))))
       (summer-autumn
	(sns ((grassland-low-sns 40) (grassland-med-sns 15) (grassland-high-sns 0))))
       (clover 0)
       )))
    (rye ;; in forage section but becky put it under grass
     (soil
      ((sandyshallow ;; light sand
	(sns ((0 160) (1  110) (2  60) (3  20) (4  0) (5   0) (6   0))))
       (organic
	(sns ((0   NA) (1   NA) (2   NA) (3  60) (4  20) (5  0) (6   0)))) 
       (peat
	(sns ((0   NA) (1   NA) (2   NA) (3   NA) (4   NA) (5  20) (6  20))))
       (default ;; other mineral soils
	 (sns ((0 NA) (1 160) (2 110) (3  60) (4  20) (5  0) (6   0))))
       )))))
      
(define crop-requirements-n-tree
  (dtree 'crop
	 (list 
	  (choice 'spring-barley-incorporated-feed barley-nitrogen-tree-feed)
          (choice 'spring-barley-incorporated-malt barley-nitrogen-tree-malt)
	  (choice 'spring-barley-removed-feed barley-nitrogen-tree-feed)
          (choice 'spring-barley-removed-malt barley-nitrogen-tree-malt)
	  (choice 'winter-wheat-incorporated-feed wheat-nitrogen-tree-feed)
          (choice 'winter-wheat-incorporated-mill wheat-nitrogen-tree-mill)
	  (choice 'winter-wheat-removed-feed wheat-nitrogen-tree-feed)
          (choice 'winter-wheat-removed-mill wheat-nitrogen-tree-mill)  
	  (choice 'grass grass-nitrogen-tree))))

(define winter-wheat-incorporated-pk-tree ;;2017 update both feed and malt should use this table 
  '(nutrient
    ((phosphorous
      (p-index ((soil-p-0 120) (soil-p-1  90) (soil-p-2  60) (soil-p-3   0))))
     (potassium
      (k-index ((soil-k-0 105) (soil-k-1  75) (soil-k-2- 45) (soil-k-2+ 20) (soil-k-3   0)))))))

(define spring-barley-incorporated-pk-tree ;;2017 update both feed and malt should use this table 
  '(nutrient
    ((phosphorous
      (p-index ((soil-p-0 105) (soil-p-1  75) (soil-p-2  45) (soil-p-3   0))))
     (potassium
      (k-index ((soil-k-0  95) (soil-k-1  65) (soil-k-2- 35) (soil-k-2+  0) (soil-k-3   0)))))))

(define winter-wheat-removed-pk-tree ;;2017 update both feed and mill should use this table
  '(nutrient
    ((phosphorous
      (p-index ((soil-p-0 125) (soil-p-1  95) (soil-p-2  65) (soil-p-3   0))))
     (potassium
      (k-index ((soil-k-0 145) (soil-k-1 115) (soil-k-2- 85) (soil-k-2+ 55) (soil-k-3   0)))))))

(define spring-barley-removed-pk-tree ;;2017 update both feed and malt should use this table 
  '(nutrient
    ((phosphorous
      (p-index ((soil-p-0 110) (soil-p-1  80) (soil-p-2  50) (soil-p-3   0))))
     (potassium
      (k-index ((soil-k-0 130) (soil-k-1 100) (soil-k-2- 70) (soil-k-2+ 40) (soil-k-3   0)))))))

(define grass-pk-tree ;;2017 update
  '(subtype
    ((silage
      (cut
       ((1
         (nutrient
          ((phosphorous
            (p-index ((soil-p-0 100) (soil-p-1  70) (soil-p-2  40) (soil-p-3   20))))
           (potassium
            (k-index ((soil-k-0 80) (soil-k-1 80) (soil-k-2- 80) (soil-k-2+ 20) (soil-k-3   30)))))))) ;; not sure how to deal with potash previous autumn/spring so used spring numbers
        (2
         (nutrient
          ((phosphorous
            (p-index ((soil-p-0 25) (soil-p-1  25) (soil-p-2  25) (soil-p-3   0))))
           (potassium
            (k-index ((soil-k-0 120) (soil-k-1 100) (soil-k-2- 90) (soil-k-2+ 60) (soil-k-3   40)))))))
        (3
         (nutrient
          ((phosphorous
            (p-index ((soil-p-0 15) (soil-p-1  15) (soil-p-2  15) (soil-p-3   0))))
           (potassium
            (k-index ((soil-k-0 80) (soil-k-1 80) (soil-k-2- 80) (soil-k-2+ 40) (soil-k-3   20)))))))
        (4
         (nutrient
          ((phosphorous
            (p-index ((soil-p-0 10) (soil-p-1  10) (soil-p-2  10) (soil-p-3   0))))
           (potassium
            (k-index ((soil-k-0 70) (soil-k-1 70) (soil-k-2- 70) (soil-k-2+ 40) (soil-k-3   20))))))))))
      (grazed
       (nutrient
       ((phosphorous
         (p-index ((soil-p-0 80) (soil-p-1  50) (soil-p-2  20) (soil-p-3   0))))
        (potassium
         (k-index ((soil-k-0 60) (soil-k-1 30) (soil-k-2- 0) (soil-k-2+ 0) (soil-k-3   0)))))))  
      (hay
       (nutrient
       ((phosphorous
         (p-index ((soil-p-0 80) (soil-p-1  50) (soil-p-2  30) (soil-p-3   0))))
        (potassium
         (k-index ((soil-k-0 140) (soil-k-1 115) (soil-k-2- 90) (soil-k-2+ 65) (soil-k-3   20)))))))
      (established
       (nutrient
       ((phosphorous
         (p-index ((soil-p-0 120) (soil-p-1  80) (soil-p-2  50) (soil-p-3   30))))
        (potassium
         (k-index ((soil-k-0 120) (soil-k-1 80) (soil-k-2- 60) (soil-k-2+ 40) (soil-k-3   0)))))))
      (rye ;; in forage section but becky put it under grass
       (nutrient
       ((phosphorous
         (p-index ((soil-p-0 90) (soil-p-1  60) (soil-p-2  30) (soil-p-3   0))))
        (potassium
         (k-index ((soil-k-0 150) (soil-k-1 120) (soil-k-2- 90) (soil-k-2+ 60) (soil-k-3   0)))))))))
      
(define crop-requirements-pk-tree 
  (dtree 
   'crop
   (list
    (choice 'winter-wheat-incorporated-feed winter-wheat-incorporated-pk-tree)
    (choice 'winter-wheat-incorporated-mill winter-wheat-incorporated-pk-tree)
    (choice 'spring-barley-incorporated-feed spring-barley-incorporated-pk-tree)
    (choice 'spring-barley-incorporated-malt spring-barley-incorporated-pk-tree)
    (choice 'winter-wheat-removed-feed winter-wheat-removed-pk-tree)
    (choice 'winter-wheat-removed-mill winter-wheat-removed-pk-tree)
    (choice 'spring-barley-removed-feed spring-barley-removed-pk-tree) 
    (choice 'spring-barley-removed-malt spring-barley-removed-pk-tree)
    (choice 'grass grass-pk-tree))))


     


