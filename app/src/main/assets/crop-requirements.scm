;;#lang scheme

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

(define grass-nitrogen-tree ;;2017 update
  '(subtype
    ((silage 
      (targetyield
       ;; cut turns out not to matter for this part as it's total for whole year
       ((DM5-7
         (cut ((one 70) (two 70) (three 70) (four 70))))
        (DM7-9
         (cut ((one 130) (two 130) (three 130) (four 130))))
        (DM9-12
         (cut ((one 250) (two 250) (three 250) (four 250))))  
        (DM12-15+
         (cut ((one 310) (two 310) (three 310) (four 310))))
        )))
      (grazed
       (targetyield
        ((DM4-5
	  ;; date is application date
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
        ((spring-sown
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
           (sns ((0 NA) (1 160) (2 110) (3  60) (4  20) (5  0) (6   0))))))))))

(define barley-spring-feed-nitrogen-tree
  '(soil
    ((sandyshallow ;; light sand
      (sns ((0 140) (1  110) (2  70) (3  50) (4  20) (5   0) (6   0)))) ;; 2017 update
     (organic
      (sns ((0   0) (1   0) (2   0) (3  70) (4  30) (5  15) (6   0))))
     (peat
      (sns ((0   0) (1   0) (2   0) (3   0) (4   0) (5  15) (6  15))))
     (default ;; other mineral soils
       (sns ((0 160) (1 140) (2 110) (3  70) (4  30) (5  15) (6   0)))))))

(define barley-spring-malt-nitrogen-tree ;;2017 update
  '(soil
    ((sandyshallow ;; light sand
      (sns ((0 110) (1  80) (2  40) (3  20) (4  0) (5   0) (6   0)))) ;; 2017 update
     (organic
      (sns ((0   0) (1   0) (2   0) (3  40) (4  15) (5  0) (6   0)))) ;;2017 update
     (peat
      (sns ((0   0) (1   0) (2   0) (3   0) (4   0) (5  0) (6  0)))) ;;2017 update
     (default ;; other mineral soils
       (sns ((0 130) (1 110) (2 70) (3  40) (4  15) (5  0) (6   0))))))) ;;2017 update

(define barley-spring-wholecrop-nitrogen-tree ;; 2017 update
  '(soil
    ((sandyshallow ;; light sand
      (sns ((0 140) (1  110) (2  70) (3  50) (4  20) (5   0) (6   0)))) ;; 2017 update
     (organic
      (sns ((0   NA) (1   NA) (2   NA) (3  70) (4  30) (5  15) (6   0)))) ;;2017 update
     (peat
      (sns ((0   NA) (1   NA) (2   NA) (3   NA) (4   NA) (5  15) (6  15)))) ;;2017 update
     (default ;; other mineral soils
       (sns ((0 160) (1 140) (2 110) (3  70) (4  30) (5  15) (6   0))))))) ;;2017 update

(define barley-winter-malt-nitrogen-tree ;; 2017 update
  '(soil
    ((sandyshallow ;; light sand
      (sns ((0 130) (1  90) (2  60) (3  20) (4  0) (5   0) (6   0)))) ;; 2017 update
     (organic
      (sns ((0   NA) (1   NA) (2   NA) (3  50) (4  20) (5  0) (6   0)))) ;;2017 update
     (peat
      (sns ((0   NA) (1   NA) (2   NA) (3   NA) (4   NA) (5  0) (6  0)))) ;;2017 update
     (default ;; other mineral soils
       (sns ((0 150) (1 120) (2 90) (3  50) (4  20) (5  0) (6   0))))))) ;;2017 update

(define barley-winter-wholecrop-nitrogen-tree ;;2017 update
  '(soil
    ((sandyshallow ;; light sand   
      (sns ((0 170) (1 140) (2 110) (3  80) (4  60) (5  20) (6  0)))) ;;2017 update
     (mediumshallow ;; shallow soils
      (sns ((0 220) (1 190) (2 150) (3 120) (4 60) (5  40) (6  10)))) ;;2017 update
     (medium
      (sns ((0 190) (1 170) (2 140) (3 110) (4 60) (5  40) (6  10)))) ;;2017 update
     (deepclay
       (sns ((0 190) (1 170) (2 140) (3 110) (4 60) (5  40) (6  10)))) ;;2017 update
     (deepsilt
      (sns ((0 170) (1 150) (2 120) (3 80) (4 40) (5  15) (6  0)))) ;; 2017 update
     (organic
      (sns ((0 NA) (1 NA) (2 NA) (3 110) (4 60) (5 20) (6 0)))) ;;2017 update
     (peat
      (sns ((0 NA) (1 NA) (2 NA) (3 NA) (4 NA) (5 20) (6 20))))))) ;; 2017 update

 (define wheat-winter-feed-nitrogen-tree
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

(define wheat-winter-mill-nitrogen-tree ;;2017 update just adds 40kg N/ha to feed table above BUT in book it says 60kg - 40kg is correct value to use
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

(define wheat-winter-wholecrop-nitrogen-tree
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
      (sns ((0   0) (1   0) (2   0) (3   0) (4   0) (5  30) (6  30))))))) ;; 2017 update

(define wheat-spring-wholecrop-nitrogen-tree
  '(soil
    ((sandyshallow ;; light sand
      (sns ((0 160) (1  130) (2  100) (3  70) (4  40) (5   20) (6   0)))) ;; 2017 update
     (organic
      (sns ((0   NA) (1   NA) (2   NA) (3  120) (4  70) (5  40) (6   20)))) ;;2017 update
     (peat
      (sns ((0   NA) (1   NA) (2   NA) (3   NA) (4   NA) (5  20) (6  20)))) ;;2017 update
     (default ;; other mineral soils
       (sns ((0 210) (1 180) (2 150) (3  120) (4  70) (5  40) (6   20))))))) ;;2017 update

(define wheat-spring-mill-nitrogen-tree ;; 2017 update - add 40 to spring wholecrop table
  '(soil
    ((sandyshallow ;; light sand
      (sns ((0 200) (1  170) (2  140) (3  110) (4  80) (5   60) (6   40)))) ;; 2017 update
     (organic
      (sns ((0   NA) (1   NA) (2   NA) (3  160) (4  110) (5  80) (6   60)))) ;;2017 update
     (peat
      (sns ((0   NA) (1   NA) (2   NA) (3   NA) (4   NA) (5  60) (6  60)))) ;;2017 update
     (default ;; other mineral soils
       (sns ((0 250) (1 220) (2 190) (3  160) (4  110) (5  80) (6   60))))))) ;;2017 update

(define triticale-winter-forage-nitrogen-tree ;;2017 update
  '(soil
    ((sandyshallow ;; light sand   
      (sns ((0 130) (1 100) (2 70) (3  40) (4  15) (5  0) (6  0)))) ;;2017 update
     (mediumshallow ;; shallow soils
      (sns ((0 230) (1 190) (2 160) (3 130) (4 90) (5  30) (6  0)))) ;;2017 update
     (medium
      (sns ((0 200) (1 170) (2 140) (3 110) (4 70) (5  15) (6  0)))) ;;2017 update
     (deepclay
      (sns ((0 200) (1 170) (2 140) (3 110) (4 70) (5  15) (6  0)))) ;;2017 update
     (deepsilt
      (sns ((0 190) (1 160) (2 120) (3 80) (4 50) (5  15) (6  0)))) ;; 2017 update
     (organic
      (sns ((0 NA) (1 NA) (2 NA) (3 70) (4 30) (5 15) (6 0)))) ;;2017 update
     (peat
      (sns ((0 NA) (1 NA) (2 NA) (3 NA) (4 NA) (5 15) (6 15))))))) ;; 2017 update


(define oat-rye-triticale-spring-nitrogen-tree ;;2017 update
    '(soil
    ((sandyshallow ;; light sand
      (sns ((0 90) (1  60) (2  30) (3  15) (4  0) (5   0) (6   0)))) ;; 2017 update
     (organic
      (sns ((0   NA) (1   NA) (2   NA) (3  40) (4  15) (5  0) (6   0)))) ;;2017 update
     (peat
      (sns ((0   NA) (1   NA) (2   NA) (3   NA) (4   NA) (5  0) (6  0)))) ;;2017 update
     (default ;; other mineral soils
       (sns ((0 140) (1 110) (2 70) (3  40) (4  15) (5  0) (6   0))))))) ;;2017 update

(define oat-winter-nitrogen-tree ;; 2017 update
    '(soil
    ((sandyshallow ;; light sand
      (sns ((0 150) (1  110) (2  80) (3  40) (4  20) (5   0) (6   0)))) ;; 2017 update
     (organic
      (sns ((0   NA) (1   NA) (2   NA) (3  100) (4  70) (5  20) (6   0)))) ;;2017 update
     (peat
      (sns ((0   NA) (1   NA) (2   NA) (3   NA) (4   NA) (5  20) (6  20)))) ;;2017 update
     (default ;; other mineral soils
       (sns ((0 190) (1 160) (2 130) (3  100) (4  70) (5  20) (6   0))))))) ;;2017 update

(define rye-winter-nitrogen-tree ;; 2017 update
  '(soil
    ((sandyshallow ;; light sand
      (sns ((0 110) (1 70) (2 35) (3 10) (4 0) (5 0) (6 0)))) ;; 2017 update
     (organic
      (sns ((0 NA) (1 NA) (2 NA) (3 60) (4 30) (5 10) (6 10)))) ;;2017 update
     (peat
      (sns ((0 NA) (1 NA) (2 NA) (3 NA) (4 NA) (5 10) (6 10)))) ;;2017 update
     (default ;; other mineral soils
       (sns ((0 150) (1 120) (2 90) (3 60) (4 30) (5 10) (6 10))))))) ;;2017 update

(define maize-nitrogen-tree ;; 2017 update
  '(sns ((0 150) (1 100) (2 50) (3 20) (4 0) (5 0) (6 0))))

(define swede-turnip-forage-lifted-nitrogen-tree ;; 2017 update
  '(sns ((0 100) (1 80) (2 60) (3 40) (4 20) (5 0) (6 0))))

(define swede-vegetable-nitrogen-tree ;; 2017 update
   '(sns ((0 135) (1 100) (2 70) (3 30) (4 0) (5 0) (6 0))))

(define turnip-nitrogen-tree ;; 2017 update
  '(sns ((0 170) (1 130) (2 100) (3 70) (4 20) (5 0) (6 0))))

(define rape-swede-turnip-forage-grazed-nitrogen-tree;; 2017 update
  '(sns ((0 75) (1 75) (2 60) (3 40) (4 20) (5 0) (6 0)))) ;; becky says to follow footnote for sns 0 and 1 which says max 75

(define rape-oilseed-spring-nitrogen-tree ;;2017 update
  '(soil
    ((sandyshallow ;; light sand
      (sns ((0 120) (1 80) (2 50) (3 20) (4 0) (5 0) (6 0)))) ;; 2017 update
     (organic
      (sns ((0 NA) (1 NA) (2 NA) (3 50) (4 20) (5 0) (6 0)))) ;;2017 update
     (peat
      (sns ((0 NA) (1 NA) (2 NA) (3 NA) (4 NA) (5 20) (6 20)))) ;;2017 update
     (default ;; other mineral soils
       (sns ((0 150) (1 120) (2 80) (3 50) (4 20) (5 0) (6 0))))))) ;;2017 update

(define rape-oilseed-winter-nitrogen-tree ;; 2017 update - this is oilseed that has been sown in winter, but the N requirements differ depending on the season that spreading happens
  '(season
    ((spring
      (soil
       ((organic
         (sns ((0 NA) (1 NA) (2 NA) (3 120) (4 80) (5 60) (6 20)))) ;;2017 update
        (peat
         (sns ((0 NA) (1 NA) (2 NA) (3 NA) (4 NA) (5 60) (6 60)))) ;;2017 update
        (default ;; other mineral soils
          (sns ((0 220) (1 190) (2 160) (3 120) (4 80) (5 60) (6 20)))))))
     (autumn
      (sns ((0 30) (1 30) (2 30) (3 0) (4 0) (5 0) (6 0))))
     (summer NA)
     (winter NA))))

(define linseed-nitrogen-tree ;; 2017 update
  '(soil
    ((sandyshallow ;; light sand
      (sns ((0 80) (1 50) (2 20) (3 00) (4 0) (5 0) (6 0)))) ;; 2017 update
     (organic
      (sns ((0 NA) (1 NA) (2 NA) (3 20) (4 0) (5 0) (6 0)))) ;;2017 update
     (peat
      (sns ((0 NA) (1 NA) (2 NA) (3 NA) (4 NA) (5 0) (6 0)))) ;;2017 update
     (default ;; other mineral soils
       (sns ((0 100) (1 80) (2 50) (3 20) (4 0) (5 0) (6 0))))))) ;;2017 update

(define pea-bean-nitrogen-tree ;; 2017 update
    '(sns ((0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (6 0))))

(define fodder-beet-nitrogen-tree;; 2017 update
  '(sns ((0 130) (1 120) (2 110) (3 90) (4 60) (5 20) (6 0))))

(define kale-grazed-nitrogen-tree;; 2017 update
  '(sns ((0 130) (1 120) (2 110) (3 90) (4 60) (5 20) (6 0))))

(define sprout-cabbage-nitrogen-tree ;; 2017 update
  '(subtype1
    ((brussels-sprout
      (sns ((0 330) (1 300) (2 270) (3 230) (4 180) (5 80) (6 0))))
     (storage-cabbage
      (sns ((0 340) (1 310) (2 280) (3 240) (4 190) (5 90) (6 0))))
     (head-cabbage-pre-31-dec
      (sns ((0 325) (1 290) (2 260) (3 220) (4 170) (5 70) (6 0))))
     (head-cabbage-post-31-dec
      (sns ((0 240) (1 210) (2 180) (3 140) (4 90) (5 0) (6 0))))
     (collard-pre-31-dec
      (sns ((0 210) (1 190) (2 180) (3 160) (4 140) (5 90) (6 0))))
     (collard-post-31-dec
      (sns ((0 310) (1 290) (2 270) (3 240) (4 210) (5 140) (6 90)))))))

(define cauliflower-calabrese-nitrogen-tree ;; 2017 update
  '(subtype1
    ((cauliflower-summer-autumn
      (sns ((0 290) (1 260) (2 235) (3 210) (4 170) (5 80) (6 0))))
     (cauliflower-winter-hardy-roscoff
      (subtype2
       ((seedbed
         (sns ((0 100) (1 100) (2 100) (3 100) (4 60) (5 0) (6 0))))
        (top-dressing
         (sns ((0 190) (1 160) (2 135) (3 110) (4 100) (5 80) (6 0)))))))
     (calabrese
      (sns ((0 235) (1 200) (2 165) (3 135) (4 80) (5 0) (6 0)))))))

(define lettuce-leafy-salad-nitrogen-tree ;; 2017 update
  '(subtype1
    ((lettuce-whole-head
      (sns ((0 200) (1 180) (2 160) (3 150) (4 125) (5 75) (6 30))))
     (lettuce-baby-leaf
      (sns ((0 60) (1 50) (2 40) (3 30) (4 10) (5 0) (6 0))))
     (wild-rocket
      (sns ((0 125) (1 115) (2 100) (3 90) (4 75) (5 40) (6 0)))))))

(define onion-leek-nitrogen-tree ;; 2017 update
  '(subtype1
    ((bulb-onion
      (sns ((0 160) (1 130) (2 110) (3 90) (4 60) (5 0) (6 0))))
     (salad-onion
      (sns ((0 130) (1 120) (2 110) (3 100) (4 80) (5 50) (6 20))))
     (leek
      (sns ((0 200) (1 190) (2 170) (3 160) (4 130) (5 80) (6 40)))))))

(define beetroot-nitrogen-tree ;; 2017 update
  '(sns ((0 290) (1 260) (2 240) (3 220) (4 190) (5 120) (6 60))))

(define carrot-nitrogen-tree ;; 2017 update
   '(sns ((0 100) (1 70) (2 40) (3 0) (4 0) (5 0) (6 0)))) 

(define bulb-nitrogen-tree ;; 2017 update
  '(sns ((0 125) (1 100) (2 50) (3 0) (4 0) (5 0) (6 0))))

(define potato-nitrogen-tree ;; 2017 update
  '(determinancy-group
    ((one
      (growing-season-length
       ((<60
         (sns ((0 120) (1 120) (2 90) (3 90) (4 90) (5 50) (6 50))))
        (60-90
         (sns ((0 185) (1 185) (2 145) (3 145) (4 145) (5 105) (6 105))))
        (90-120
         (sns ((0 245) (1 245) (2 205) (3 205) (4 205) (5 165) (6 164))))
        (>120
         (sns ((0 NA) (1 NA) (2 NA) (3 NA) (4 NA) (5 NA) (6 NA)))))))
     (two
      (growing-season-length
       ((<60
         (sns ((0 100) (1 100) (2 65) (3 65) (4 65) (5 20) (6 20))))
        (60-90
         (sns ((0 130) (1 130) (2 90) (3 90) (4 90) (5 60) (6 60))))
        (90-120
         (sns ((0 185) (1 185) (2 135) (3 135) (4 135) (5 100) (6 100))))
        (>120
         (sns ((0 220) (1 220) (2 165) (3 165) (4 165) (5 135) (6 135)))))))
     (three
      (growing-season-length
       ((<60
         (sns ((0 80) (1 80) (2 55) (3 55) (4 55) (5 20) (6 20))))
        (60-90
         (sns ((0 100) (1 100) (2 70) (3 70) (4 70) (5 30) (6 30))))
        (90-120
         (sns ((0 145) (1 145) (2 90) (3 90) (4 90) (5 50) (6 50))))
        (>120
         (sns ((0 180) (1 180) (2 130) (3 130) (4 130) (5 90) (6 90)))))))
     (four
      (growing-season-length
       ((<60
         (sns ((0 NA) (1 NA) (2 NA) (3 NA) (4 NA) (5 NA) (6 NA))))
        (60-90
         (sns ((0 60) (1 60) (2 30) (3 30) (4 30) (5 20) (6 20))))
        (90-120
         (sns ((0 110) (1 110) (2 50) (3 50) (4 50) (5 20) (6 20))))
        (>120
         (sns ((0 140 ) (1 140) (2 70) (3 70) (4 70) (5 30) (6 30))))))))))


(define grass-pk-tree ;;2017 update
  '(subtype
    ((silage
      ;; added up all the cuts to give values for the whole year
      (nutrient
       ((phosphorus
	 (p-index ((soil-p-0 150) (soil-p-1 120) (soil-p-2 90) (soil-p-3 20) (soil-p-4 0))))
	(potassium
	 (k-index ((soil-k-0 410) (soil-k-1 360) (soil-k-2- 320) (soil-k-2+ 200) (soil-k-3 100) (soil-k-4 0))))
	(magnesium
	 (m-index ((soil-m-0 84) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
	(sulphur
	 (risk ((high 160) (low 0))))))) ;; potash numbers come from adding up previous autumn and spring values to make a total for the season
          
     ;; (cut
     ;;  ((one
     ;;    (nutrient
     ;;     ((phosphorus
     ;;       (p-index ((soil-p-0 100) (soil-p-1  70) (soil-p-2  40) (soil-p-3 20) (soil-p-4 0))))
     ;;      (potassium
     ;;       (k-index ((soil-k-0 140) (soil-k-1 110) (soil-k-2- 80) (soil-k-2+ 60) (soil-k-3 30) (soil-k-4 0))))
     ;;      (magnesium
     ;;       (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     ;;      (sulphur
     ;;       (risk ((high 40) (low 0))))))) ;; potash numbers come from adding up previous autumn and spring values to make a total for the season
     ;; 	(two
     ;;   (nutrient
     ;;    ((phosphorus
     ;;      (p-index ((soil-p-0 25) (soil-p-1  25) (soil-p-2  25) (soil-p-3   0) (soil-p-4 0))))
     ;;     (potassium
     ;;      (k-index ((soil-k-0 120) (soil-k-1 100) (soil-k-2- 90) (soil-k-2+ 60) (soil-k-3   40) (soil-k-4 0))))
     ;;     (magnesium
     ;;      (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     ;;     (sulphur
     ;;       (risk ((high 40) (low 0)))))))
     ;;  (three
     ;;   (nutrient
     ;; 	 ((phosphorus
     ;;      (p-index ((soil-p-0 15) (soil-p-1  15) (soil-p-2  15) (soil-p-3   0) (soil-p-4 0))))
     ;;     (potassium
     ;;      (k-index ((soil-k-0 80) (soil-k-1 80) (soil-k-2- 80) (soil-k-2+ 40) (soil-k-3   20))))
     ;;     (magnesium
     ;;      (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     ;;     (sulphur
     ;;       (risk ((high 40) (low 0)))))))
     ;;  (four
     ;;   (nutrient
     ;;    ((phosphorus
     ;;      (p-index ((soil-p-0 10) (soil-p-1  10) (soil-p-2  10) (soil-p-3   0) (soil-p-4 0))))
     ;;     (potassium
     ;;      (k-index ((soil-k-0 70) (soil-k-1 70) (soil-k-2- 70) (soil-k-2+ 40) (soil-k-3   20) (soil-k-4 0))))
     ;;     (magnesium
     ;;      (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     ;;     (sulphur
     ;;       (risk ((high 40) (low 0)))))))))
     
     (grazed
      (nutrient
       ((phosphorus
         (p-index ((soil-p-0 80) (soil-p-1  50) (soil-p-2  20) (soil-p-3   0) (soil-p-4 0))))
        (potassium
         (k-index ((soil-k-0 60) (soil-k-1 30) (soil-k-2- 0) (soil-k-2+ 0) (soil-k-3   0) (soil-k-4 0))))
        (magnesium
         (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
        (sulphur
	 (risk
	  ((low 0)
	   (high 30)
	   ;; simplified this as otherwise we need to constantly keep track
	   ;; for manure information for crop requirements for this one
	   ;; crop variation
	   ;; (nitrogencategory 
	   ;; ((under100 25) (100-200 50) (200-300 75) (300-400 100)))
	   ))))))
     (hay
      (nutrient
       ((phosphorus
         (p-index ((soil-p-0 80) (soil-p-1  50) (soil-p-2  30) (soil-p-3   0) (soil-p-4 0))))
        (potassium
         (k-index ((soil-k-0 140) (soil-k-1 115) (soil-k-2- 90) (soil-k-2+ 65) (soil-k-3   20) (soil-k-4 0))))
        (magnesium
         (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
        (sulphur
          (risk ((high NA) (low NA)))))))
     (established
      (nutrient
       ((phosphorus
         (p-index ((soil-p-0 120) (soil-p-1  80) (soil-p-2  50) (soil-p-3   30) (soil-p-4 0))))
        (potassium
         (k-index ((soil-k-0 120) (soil-k-1 80) (soil-k-2- 60) (soil-k-2+ 40) (soil-k-3   0) (soil-k-4 0))))
        (magnesium
         (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
        (sulphur
         (risk ((high NA) (low NA)))))))
     (rye ;; in forage section but becky put it under grass
      (nutrient
       ((phosphorus
         (p-index ((soil-p-0 90) (soil-p-1  60) (soil-p-2  30) (soil-p-3   0) (soil-p-4 0))))
        (potassium
         (k-index ((soil-k-0 150) (soil-k-1 120) (soil-k-2- 90) (soil-k-2+ 60) (soil-k-3   0) (soil-k-4 0))))
        (magnesium
         (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
        (sulphur
         (risk ((high NA) (low NA))))))))))

(define barley-spring-incorporated-pk-tree ;;2017 update both feed and malt should use this table 
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 105) (soil-p-1  75) (soil-p-2  45) (soil-p-3   0) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0  95) (soil-k-1  65) (soil-k-2- 35) (soil-k-2+  0) (soil-k-3   0) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 38) (low 0)))))))

(define barley-spring-removed-pk-tree ;;2017 update both feed and malt should use this table 
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 110) (soil-p-1  80) (soil-p-2  50) (soil-p-3   0) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0 130) (soil-k-1 100) (soil-k-2- 70) (soil-k-2+ 40) (soil-k-3   0) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 38) (low 0)))))))

(define wholecrop-cereal-pk-tree ;; 2018 corrections
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 115) (soil-p-1 85) (soil-p-2 55) (soil-p-3 0) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0 220) (soil-k-1 190) (soil-k-2- 160) (soil-k-2+ 160) (soil-k-3 100) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 38) (low 0)))))))

(define wheat-winter-removed-pk-tree ;;2017 update both feed and mill should use this table
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 125) (soil-p-1  95) (soil-p-2  65) (soil-p-3   0) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0 145) (soil-k-1 115) (soil-k-2- 85) (soil-k-2+ 55) (soil-k-3   0) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 38) (low 0)))))))

(define wheat-winter-incorporated-pk-tree ;;2017 update both feed and malt should use this table 
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 120) (soil-p-1  90) (soil-p-2  60) (soil-p-3   0) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0 105) (soil-k-1  75) (soil-k-2- 45) (soil-k-2+ 20) (soil-k-3   0) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 38) (low 0)))))))

(define oat-removed-pk-tree ;;2017 update
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 115) (soil-p-1 85) (soil-p-2 55) (soil-p-3 0) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0 165) (soil-k-1 135) (soil-k-2- 105) (soil-k-2+ 75) (soil-k-3 0) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 38) (low 0)))))))

(define oat-incorporated-pk-tree ;;2017 update
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 105) (soil-p-1 75) (soil-p-2 45) (soil-p-3 0) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0 95) (soil-k-1 65) (soil-k-2- 35) (soil-k-2+ 0) (soil-k-3 0) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 38) (low 0)))))))

(define maize-pk-tree ;; 2017 update
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 115) (soil-p-1 85) (soil-p-2 55) (soil-p-3 20) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0 235) (soil-k-1 205) (soil-k-2- 175) (soil-k-2+ 145) (soil-k-3 110) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 38) (low 0)))))))

(define swede-turnip-foraged-lifted-pk-tree ;; 2017 update
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 105) (soil-p-1 75) (soil-p-2 45) (soil-p-3 0) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0 215) (soil-k-1 185) (soil-k-2- 155) (soil-k-2+ 125) (soil-k-3 80) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 25) (low 0)))))))

(define rape-swede-turnip-forage-grazed-pk-tree ;; 2017 update
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 85) (soil-p-1 55) (soil-p-2 25) (soil-p-3 0) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0 110) (soil-k-1 80) (soil-k-2- 50) (soil-k-2+ 20) (soil-k-3 0) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 25) (low 0)))))))

(define rape-oilseed-spring-pk-tree ;; 2017 update
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 90) (soil-p-1 60) (soil-p-2 30) (soil-p-3 0) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0 80) (soil-k-1 50) (soil-k-2- 20) (soil-k-2+ 0) (soil-k-3 0) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 21) (soil-m-1 21) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (soil ((organic 0) (peat 0) (default 63))))))) 

(define rape-oilseed-winter-pk-tree ;; 2017 update
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 110) (soil-p-1 80) (soil-p-2 50) (soil-p-3 0) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0 100) (soil-k-1 70) (soil-k-2- 40) (soil-k-2+ 20) (soil-k-3 0) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 21) (soil-m-1 21) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur (soil ((organic 0) (peat 0) (default 63)))))))

(define pea-bean-pk-tree ;; 2017 update
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 100) (soil-p-1 70) (soil-p-2 40) (soil-p-3 0) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0 100) (soil-k-1 70) (soil-k-2- 40) (soil-k-2+ 20) (soil-k-3 0) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 100) (soil-m-1 50) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 25) (low 0)))))))  
  
(define fodder-beet-pk-tree ;; 2017 update
    '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 120) (soil-p-1 90) (soil-p-2 60) (soil-p-3 0) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0 400) (soil-k-1 370) (soil-k-2- 340) (soil-k-2+ 310) (soil-k-3 0) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 150) (soil-m-1 75) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 25) (low 0)))))))

(define kale-grazed-pk-tree ;; 2017 update
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 80) (soil-p-1 50) (soil-p-2 20) (soil-p-3 0) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0 200) (soil-k-1 170) (soil-k-2- 140) (soil-k-2+ 70) (soil-k-3 70) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 21) (soil-m-1 0) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 25) (low 0)))))))

(define sprout-cabbage-pk-tree ;; 2017 update
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 200) (soil-p-1 150) (soil-p-2 100) (soil-p-3 50) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0 300) (soil-k-1 250) (soil-k-2- 200) (soil-k-2+ 150) (soil-k-3 60) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 150) (soil-m-1 100) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 63) (low 0)))))))

(define cauliflower-calabrese-pk-tree ;; 2017 update
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 200) (soil-p-1 150) (soil-p-2 100) (soil-p-3 50) (soil-p-4 0))))
     (potassium
      (k-index ((soil-k-0 275) (soil-k-1 225) (soil-k-2- 175) (soil-k-2+ 125) (soil-k-3 35) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 150) (soil-m-1 100) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 63) (low 0)))))))

(define lettuce-leafy-salad-pk-tree ;; 2017 update
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 250) (soil-p-1 200) (soil-p-2 150) (soil-p-3 100) (soil-p-4 0)))) ;; in notes it says at p4/5 60kg may be useful, but there are caveats so left at 0
     (potassium
      (k-index ((soil-k-0 250) (soil-k-1 200) (soil-k-2- 150) (soil-k-2+ 100) (soil-k-3 0) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 150) (soil-m-1 100) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 25) (low 0)))))))

(define onion-leek-pk-tree ;; 2017 update
    '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 200) (soil-p-1 150) (soil-p-2 100) (soil-p-3 50) (soil-p-4 0)))) 
     (potassium
      (k-index ((soil-k-0 275) (soil-k-1 225) (soil-k-2- 175) (soil-k-2+ 125) (soil-k-3 35) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 150) (soil-m-1 100) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 25) (low 0)))))))

(define beetroot-pk-tree ;; 2017 update
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 200) (soil-p-1 150) (soil-p-2 100) (soil-p-3 50) (soil-p-4 0)))) 
     (potassium
      (k-index ((soil-k-0 300) (soil-k-1 250) (soil-k-2- 200) (soil-k-2+ 150) (soil-k-3 60) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 150) (soil-m-1 100) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 25) (low 0)))))))

(define carrot-pk-tree ;; 2017 update
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 200) (soil-p-1 150) (soil-p-2 100) (soil-p-3 50) (soil-p-4 0)))) 
     (potassium
      (k-index ((soil-k-0 275) (soil-k-1 225) (soil-k-2- 175) (soil-k-2+ 125) (soil-k-3 35) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 150) (soil-m-1 100) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 25) (low 0)))))))

(define bulb-pk-tree ;; 2017 update
  '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 200) (soil-p-1 150) (soil-p-2 100) (soil-p-3 50) (soil-p-4 0)))) 
     (potassium
      (k-index ((soil-k-0 300) (soil-k-1 250) (soil-k-2- 200) (soil-k-2+ 150) (soil-k-3 60) (soil-k-4 0)))) ;; typo in book for 2+, correction document says 150
     (magnesium
      (m-index ((soil-m-0 150) (soil-m-1 100) (soil-m-2 0) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 25) (low 0)))))))

(define potato-pk-tree ;; 2017 update
    '(nutrient
    ((phosphorus
      (p-index ((soil-p-0 250) (soil-p-1 210) (soil-p-2 170) (soil-p-3 100) (soil-p-4 0)))) 
     (potassium
      (k-index ((soil-k-0 360) (soil-k-1 330) (soil-k-2- 300) (soil-k-2+ 300) (soil-k-3 150) (soil-k-4 0))))
     (magnesium
      (m-index ((soil-m-0 120) (soil-m-1 80) (soil-m-2 40) (soil-m-3 0) (soil-m-4 0) (soil-m-5 0) (soil-m-6 0) (soil-m-7 0) (soil-m-8 0) (soil-m-9 0))))
     (sulphur
      (risk ((high 25) (low 0)))))))

(define barley-nitrogen-tree
  (dtree 'sown
	 (list
	  (choice 'spring
		  (dtree 'application
			 (list
			  (choice 'incorporated
				  (dtree 'process
					 (list
					  (choice 'feed barley-spring-feed-nitrogen-tree)
					  (choice 'malt barley-spring-malt-nitrogen-tree))))
			  (choice 'removed
				  (dtree 'process
					 (list
					  (choice 'feed barley-spring-feed-nitrogen-tree) ;; ?? was barley-nitrogen-tree-feed
					  (choice 'malt barley-spring-malt-nitrogen-tree)))) ;; ?? was barley-nitrogen-tree-malt
			  (choice 'wholecrop barley-spring-wholecrop-nitrogen-tree))))
	  (choice 'winter
		  (dtree 'application
			 (list
			  (choice 'incorporated
				  (dtree 'process
					 (list
					  (choice 'feed barley-winter-wholecrop-nitrogen-tree) 
					  (choice 'malt barley-winter-malt-nitrogen-tree))))
			  (choice 'removed
				  (dtree 'process
					 (list
					  (choice 'feed barley-winter-wholecrop-nitrogen-tree)
					  (choice 'malt barley-winter-malt-nitrogen-tree))))
			  (choice 'wholecrop barley-winter-wholecrop-nitrogen-tree)))))))

(define wheat-nitrogen-tree
  (dtree 'sown
	 (list
	  (choice 'winter
		  (dtree 'application
			 (list
			  (choice 'incorporated
				  (dtree 'process
					 (list
					  (choice 'feed wheat-winter-feed-nitrogen-tree)
					  (choice 'mill wheat-winter-mill-nitrogen-tree))))
			  (choice 'removed
				  (dtree 'process
					 (list
					  (choice 'feed wheat-winter-feed-nitrogen-tree)
					  (choice 'mill wheat-winter-mill-nitrogen-tree))))
			  (choice 'wholecrop wheat-winter-wholecrop-nitrogen-tree))))
	  (choice 'spring
		  (dtree 'application
			 (list
			  (choice 'incorporated
				  (dtree 'process
					 (list
					  (choice 'feed wheat-spring-wholecrop-nitrogen-tree) 
					  (choice 'mill wheat-spring-mill-nitrogen-tree))))
			  (choice 'removed
				  (dtree 'process
					 (list
					  (choice 'feed wheat-spring-wholecrop-nitrogen-tree)
					  (choice 'mill wheat-spring-mill-nitrogen-tree))))
			  (choice 'wholecrop wheat-spring-wholecrop-nitrogen-tree)))))))

(define triticale-nitrogen-tree
  (dtree 'sown (list
		(choice 'winter
			(dtree 'application
			       (list
				(choice 'forage triticale-winter-forage-nitrogen-tree) ;; table 3.24 p30 forage
				(choice 'incorporated wheat-winter-feed-nitrogen-tree) ;; table 4.15 cereal
				(choice 'removed wheat-winter-feed-nitrogen-tree)))) ;; table 4.15 cereal
		(choice 'spring
			(dtree 'application
			       (list
				(choice 'forage oat-rye-triticale-spring-nitrogen-tree) ;; table 3.25 p31 forage
				(choice 'incorporated oat-rye-triticale-spring-nitrogen-tree) ;; table 4.20 p 34 in cereals (same as p31 forage)
				(choice 'removed oat-rye-triticale-spring-nitrogen-tree))))))) ;; table 4.20 p 34 in cereals (same as p31 forage)

(define oat-nitrogen-tree
  (dtree 'sown 
	 (list 
	  (choice 'winter oat-winter-nitrogen-tree) 
	  (choice 'spring oat-rye-triticale-spring-nitrogen-tree))))

(define rye-nitrogen-tree  
  (dtree 'sown 
	 (list
	  (choice 'winter
		  (dtree 'application
			 (list
			  (choice 'forage rye-winter-nitrogen-tree)
			  (choice 'incorporated rye-winter-nitrogen-tree)
			  (choice 'removed rye-winter-nitrogen-tree))))
	  (choice 'spring
		  (dtree 'application
			 (list
			  (choice 'forage oat-rye-triticale-spring-nitrogen-tree)
			  (choice 'incorporated oat-rye-triticale-spring-nitrogen-tree)
			  (choice 'removed oat-rye-triticale-spring-nitrogen-tree)))))))

(define swede-nitrogen-tree
  (dtree 'use
	 (list
	  (choice 'forage-lifted swede-turnip-forage-lifted-nitrogen-tree)
	  (choice 'forage-grazed rape-swede-turnip-forage-grazed-nitrogen-tree)
	  (choice 'vegetable swede-vegetable-nitrogen-tree))))

(define turnip-nitrogen-tree
  (dtree 'use
	 (list
	  (choice 'forage-lifted swede-turnip-forage-lifted-nitrogen-tree)
	  (choice 'forage-grazed rape-swede-turnip-forage-grazed-nitrogen-tree)
	  (choice 'vegetable turnip-nitrogen-tree))))


(define rape-nitrogen-tree
  (dtree 'use
	 (list
	  (choice 'forage rape-swede-turnip-forage-grazed-nitrogen-tree)
	  (choice 'oilseed
		  (dtree 'sown
			 (list
			  (choice 'winter rape-oilseed-winter-nitrogen-tree)
			  (choice 'spring rape-oilseed-spring-nitrogen-tree)))))))


(define barley-pk-tree
  (dtree 'sown
	 (list
	  (choice 'spring
		  (dtree 'application
			 (list
			  (choice 'incorporated barley-spring-incorporated-pk-tree)
			  (choice 'removed barley-spring-removed-pk-tree)
			  (choice 'wholecrop wholecrop-cereal-pk-tree))))  ;; 2018 corrections
	  (choice 'winter
		  (dtree 'application
			 (list
			  (choice 'incorporated wheat-winter-incorporated-pk-tree)
			  (choice 'removed wheat-winter-removed-pk-tree)
			  (choice 'wholecrop wholecrop-cereal-pk-tree)))))))  ;; 2018 corrections

(define wheat-pk-tree
  (dtree 'sown
	 (list 
	  (choice 'winter
		  (dtree 'application
			 (list
			  (choice 'incorporated wheat-winter-incorporated-pk-tree)
			  (choice 'removed wheat-winter-removed-pk-tree)
			  (choice 'wholecrop wholecrop-cereal-pk-tree))))
	  (choice 'spring
		  (dtree 'application
			 (list
			  (choice 'incorporated barley-spring-incorporated-pk-tree) ;; barley?
			  (choice 'removed barley-spring-removed-pk-tree) ;; barley?
			  (choice 'wholecrop wholecrop-cereal-pk-tree))))))) 		    


(define triticale-pk-tree
  (dtree 'sown 
	 (list
	  (choice 'spring (dtree 'application 
				 (list 
				  (choice 'forage wholecrop-cereal-pk-tree)  ;; 2018 corrections
				  (choice 'incorporated barley-spring-incorporated-pk-tree)  ;; p24 cereals book 
				  (choice 'removed barley-spring-removed-pk-tree))))  ;; p24 cereals book 
	  (choice 'winter (dtree 'application
				 (list
				  (choice 'forage wholecrop-cereal-pk-tree)  ;; 2018 corrections 
				  (choice 'incorporated barley-spring-incorporated-pk-tree)  ;; p24 cereals book 
				  (choice 'removed  barley-spring-removed-pk-tree)))))))  ;; p24 cereals book         

(define oat-pk-tree
  (dtree 'sown 
	 (list
	  (choice 'winter
		  (dtree 'application 
			 (list
			  (choice 'forage wholecrop-cereal-pk-tree)   ;; 2018 corrections
			  (choice 'incorporated oat-incorporated-pk-tree)
			  (choice 'removed oat-removed-pk-tree))))
	  (choice 'spring
		  (dtree 'application
			 (list
			  (choice 'forage  wholecrop-cereal-pk-tree)   ;; 2018 corrections
			  (choice 'incorporated oat-incorporated-pk-tree)
			  (choice 'removed oat-removed-pk-tree)))))))

(define rye-pk-tree
  (dtree 'sown 
	 (list
	  (choice 'spring
		  (dtree 'application
			 (list
			  (choice 'forage wholecrop-cereal-pk-tree)  ;; 2018 corrections 
			  (choice 'incorporated barley-spring-incorporated-pk-tree)
			  (choice 'removed barley-spring-removed-pk-tree))))
	  (choice 'winter
		  (dtree 'application
			 (list
			  (choice 'forage wholecrop-cereal-pk-tree)  ;; 2018 corrections
			  (choice 'incorporated barley-spring-incorporated-pk-tree)
			  (choice 'removed barley-spring-removed-pk-tree)))))))

(define swede-pk-tree
  (dtree 'use 
	 (list
	  (choice 'forage-lifted swede-turnip-foraged-lifted-pk-tree)
	  (choice 'forage-grazed rape-swede-turnip-forage-grazed-pk-tree)
	  (choice 'vegetable beetroot-pk-tree))))

(define turnip-pk-tree
  (dtree 'use
	 (list
	  (choice 'forage-lifted swede-turnip-foraged-lifted-pk-tree)
	  (choice 'forage-grazed rape-swede-turnip-forage-grazed-pk-tree)
	  (choice 'vegetable beetroot-pk-tree))))

(define rape-pk-tree
  (dtree 'use
	 (list
	  (choice 'forage rape-swede-turnip-forage-grazed-pk-tree)
	  (choice 'oilseed
		  (dtree 'sown
			 (list
			  (choice 'spring rape-oilseed-spring-pk-tree)
			  (choice 'winter rape-oilseed-winter-pk-tree)))))))


(define crop-requirements-n-tree
  (dtree 
   'crop
   (list
    (choice 'grass grass-nitrogen-tree)
    (choice 'barley barley-nitrogen-tree)
    (choice 'wheat wheat-nitrogen-tree)
    (choice 'triticale triticale-nitrogen-tree)
    (choice 'oat oat-nitrogen-tree)
    (choice 'rye rye-nitrogen-tree)
    (choice 'maize maize-nitrogen-tree)       
    (choice 'swede swede-nitrogen-tree)	 
    (choice 'turnip turnip-nitrogen-tree)
    (choice 'rape rape-nitrogen-tree)
    (choice 'linseed linseed-nitrogen-tree)
    (choice 'pea-bean pea-bean-nitrogen-tree)
    (choice 'fodder-beet fodder-beet-nitrogen-tree)
    (choice 'kale-grazed kale-grazed-nitrogen-tree)
    (choice 'brussels-sprout-cabbage sprout-cabbage-nitrogen-tree)
    (choice 'cauliflower-calabrese cauliflower-calabrese-nitrogen-tree)
    (choice 'lettuce-leafy-salad lettuce-leafy-salad-nitrogen-tree)
    (choice 'onion-leek onion-leek-nitrogen-tree)
    (choice 'beetroot beetroot-nitrogen-tree)
    (choice 'parsnip turnip-nitrogen-tree)
    (choice 'carrot carrot-nitrogen-tree)
    (choice 'bulb bulb-nitrogen-tree)
    (choice 'potato potato-nitrogen-tree))))

      
(define crop-requirements-pk-tree 
  (dtree 
   'crop
   (list
    (choice 'grass grass-pk-tree)
    (choice 'barley barley-pk-tree)
    (choice 'wheat wheat-pk-tree)
    (choice 'triticale triticale-pk-tree)
    (choice 'oat oat-pk-tree)
    (choice 'rye rye-pk-tree)
    (choice 'maize maize-pk-tree)
    (choice 'swede swede-pk-tree)
    (choice 'turnip turnip-pk-tree)
    (choice 'rape rape-pk-tree)
    (choice 'linseed rape-oilseed-spring-pk-tree)
    (choice 'pea-bean pea-bean-pk-tree)
    (choice 'fodder-beet fodder-beet-pk-tree)
    (choice 'kale-grazed kale-grazed-pk-tree)
    (choice 'brussels-sprout-cabbage sprout-cabbage-pk-tree)
    (choice 'cauliflower-calabrese cauliflower-calabrese-pk-tree)
    (choice 'lettuce-leafy-salad lettuce-leafy-salad-pk-tree)
    (choice 'onion-leek onion-leek-pk-tree)
    (choice 'beetroot beetroot-pk-tree)
    (choice 'parsnip beetroot-pk-tree)
    (choice 'carrot carrot-pk-tree)
    (choice 'bulb bulb-pk-tree)
    (choice 'potato potato-pk-tree))))
