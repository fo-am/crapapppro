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

(define wheat-nitrogen-tree
  '(soil
    ((sandyshallow ;; light sand   
      (sns ((0 160) (1 130) (2 100) (3  70) (4  40) (5  20) (6  20))))
     (mediumshallow ;; shallow soils
      (sns ((0 280) (1 240) (2 210) (3 180) (4 140) (5  80) (6  20))))
     (medium
      (sns ((0 250) (1 220) (2 190) (3 160) (4 120) (5  60) (6  20))))
     (deepclay
      (sns ((0 250) (1 220) (2 190) (3 160) (4 120) (5  60) (6  20))))
     (deepsilt
      (sns ((0 220) (1 190) (2 160) (3 130) (4 100) (5  40) (6  20))))
     (organic
      (sns ((0   0) (1   0) (2   0) (3 120) (4  80) (5  60) (6  20))))
     (peat
      (sns ((0   0) (1   0) (2   0) (3   0) (4   0) (5  30) (6  30)))))))

(define barley-nitrogen-tree
  '(soil
    ((sandyshallow ;; light sand
      (sns ((0 110) (1  80) (2  50) (3  30) (4  15) (5   0) (6   0))))
     (organic
      (sns ((0   0) (1   0) (2   0) (3  70) (4  30) (5  15) (6   0))))
     (peat
      (sns ((0   0) (1   0) (2   0) (3   0) (4   0) (5  15) (6  15))))
     (default ;; other mineral soils
       (sns ((0 160) (1 140) (2 110) (3  70) (4  30) (5  15) (6   0))))
     )))

(define crop-requirements-n-tree
  (dtree 'crop
	 (list 
	  (choice 'spring-barley-incorporated barley-nitrogen-tree)
	  (choice 'spring-barley-removed barley-nitrogen-tree)
	  (choice 'winter-wheat-incorporated wheat-nitrogen-tree)
	  (choice 'winter-wheat-removed wheat-nitrogen-tree)
	  (choice 'grass-cut 260)
	  (choice 'grass-grazed 240)))) 

(define crop-requirements-pk-tree 
  '(crop
    ((winter-wheat-incorporated 
      (nutrient
       ((phosphorous
	 (p-index ((soil-p-0 120) (soil-p-1  90) (soil-p-2  60) (soil-p-3   0))))
	(potassium
	 (k-index ((soil-k-0 105) (soil-k-1  75) (soil-k-2- 45) (soil-k-2+ 20) (soil-k-3   0)))))))
     (spring-barley-incorporated 
      (nutrient
       ((phosphorous
	 (p-index ((soil-p-0 105) (soil-p-1  75) (soil-p-2  45) (soil-p-3   0))))
	(potassium
	 (k-index ((soil-k-0  95) (soil-k-1  65) (soil-k-2- 35) (soil-k-2+  0) (soil-k-3   0)))))))
     (winter-wheat-removed
      (nutrient
       ((phosphorous
	 (p-index ((soil-p-0 125) (soil-p-1  95) (soil-p-2  65) (soil-p-3   0))))
	(potassium
	 (k-index ((soil-k-0 145) (soil-k-1 115) (soil-k-2- 85) (soil-k-2+ 55) (soil-k-3   0)))))))
     (spring-barley-removed
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
	 (k-index ((soil-k-0 80) (soil-k-1 80) (soil-p-2- 80) (soil-k-2+ 30) (soil-k-3 0)))))))
     
     ;; pp 210
     (grass-grazed 
      (nutrient 
       ((phosphorous
	 (p-index ((soil-p-0 80) (soil-p-1 50) (soil-p-2 20) (soil-p-3 0))))
	(potassium
	 (k-index ((soil-k-0 60) (soil-k-1 30) (soil-p-2- 0) (soil-k-2+ 0) (soil-k-3 0))))))))))

     


