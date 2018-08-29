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

(define barley-nitrogen-tree-malt ;;2017update
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
	  (choice 'grass-cut 260)
	  (choice 'grass-grazed 240)))) 

(define crop-requirements-pk-tree 
  '(crop
    ((winter-wheat-incorporated ;;2017 update both feed and mill should use this table 
      (nutrient
       ((phosphorous
	 (p-index ((soil-p-0 120) (soil-p-1  90) (soil-p-2  60) (soil-p-3   0))))
	(potassium
	 (k-index ((soil-k-0 105) (soil-k-1  75) (soil-k-2- 45) (soil-k-2+ 20) (soil-k-3   0)))))))
     (spring-barley-incorporated ;;2017 update both feed and malt should use this table 
      (nutrient
       ((phosphorous
	 (p-index ((soil-p-0 105) (soil-p-1  75) (soil-p-2  45) (soil-p-3   0))))
	(potassium
	 (k-index ((soil-k-0  95) (soil-k-1  65) (soil-k-2- 35) (soil-k-2+  0) (soil-k-3   0)))))))
     (winter-wheat-removed ;;2017 update both feed and mill should use this table
      (nutrient
       ((phosphorous
	 (p-index ((soil-p-0 125) (soil-p-1  95) (soil-p-2  65) (soil-p-3   0))))
	(potassium
	 (k-index ((soil-k-0 145) (soil-k-1 115) (soil-k-2- 85) (soil-k-2+ 55) (soil-k-3   0)))))))
     (spring-barley-removed ;;2017 update both feed and malt should use this table 
      (nutrient
       ((phosphorous
	 (p-index ((soil-p-0 110) (soil-p-1  80) (soil-p-2  50) (soil-p-3   0))))
	(potassium
	 (k-index ((soil-k-0 130) (soil-k-1 100) (soil-k-2- 70) (soil-k-2+ 40) (soil-k-3   0)))))))

     ;; first cut, pp 211
     (grass-cut
      (nutrient 
       ((phosphorous
	 (p-index ((soil-p-0 100) (soil-p-1 70) (soil-p-2 40) (soil-p-3 0))))
	;; spring cut values for k
	(potassium
	 (k-index ((soil-k-0 80) (soil-k-1 80) (soil-k-2- 80) (soil-k-2+ 30) (soil-k-3 0)))))))
     
     ;; pp 210
     (grass-grazed 
      (nutrient 
       ((phosphorous
	 (p-index ((soil-p-0 80) (soil-p-1 50) (soil-p-2 20) (soil-p-3 0))))
	(potassium
	 (k-index ((soil-k-0 60) (soil-k-1 30) (soil-k-2- 0) (soil-k-2+ 0) (soil-k-3 0))))))))))

     


