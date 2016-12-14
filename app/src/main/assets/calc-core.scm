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

;; these are the symbols used everywhere


(define manure-type-list (list 'cattle 'fym 'pig 'poultry 'compost 'custom-manure))
(define units-list (list 'metric 'imperial))
(define soil-type-list 
  (list 'sandyshallow 'medium 'peat 'organic 'mediumshallow 'deepclay 'deepsilt))
(define soil-test-p-list (list 'soil-p-0 'soil-p-1 'soil-p-2 'soil-p-3))
(define soil-test-k-list (list 'soil-k-0 'soil-k-1 'soil-k-2- 'soil-k-2+ 'soil-k-3))
(define crop-type-list 
  (list	'winter-wheat-removed 'winter-wheat-incorporated 
	'spring-barley-removed 'spring-barley-incorporated 
	'grass-cut 'grass-grazed))

(define previous-crop-type-list 
  (list 'cereals 'oilseed 'potatoes 
	'sugarbeet 'peas 'beans 'low-n-veg 'medium-n-veg 'forage 
	'uncropped 'grass-low-n 'grass-high-n 'grass-other))
(define season-list (list 'autumn 'winter 'spring 'summer))

(define cattle-quality-list (list 'DM2 'DM6 'DM10))
(define pig-quality-list (list 'DM2 'DM4 'DM6))
(define poultry-quality-list (list 'layer 'broiler))
(define fym-quality-list (list 'fym-cattle 'fym-pig 'fym-sheep 'fym-duck 'fym-horse 'fym-goat))
(define compost-quality-list (list 'green 'green-food))
(define yesno-list (list 'yes 'no))

(define fym-application-list
  (list 'straight-surface 'straight-ploughed 'stored-spread 'stored-ploughed))
(define pig-application-list
  (list 'splash-surface 'splash-incorporated 'shoe-bar-spreader 'shallow-injected))
(define cattle-application-list
  (list 'splash-surface 'splash-incorporated 'shoe-bar-spreader 'shallow-injected))

(define rainfall-list (list 'high 'medium 'low))

;; metric/imperial conversion
(define (tons/acre->tons/ha a) (* a 2.47105381))
(define (tons/ha->tons/acre a) (/ a 2.47105381))
(define (gallons/acre->m3/ha a) (* a 0.0112336377))
(define (m3/ha->gallons/acre a) (/ a 0.0112336377))
(define (kg/ha->units/acre a) (* a 0.8))
(define (units/acre->kg/ha a) (/ a 0.8))
(define (acres->hectares a) (* a 0.404686))
(define (hectares->acres a) (* a 2.47105))
(define (m3->gallons a) (* a 219.969))

(define (m2->hectares a) (/ a 10000))
(define (hectares->m2 a) (* a 10000))

;; overwritten by db values
(define costs (list 0 0 0))
(define custom-override #f)
(define (set-custom-override! s) (set! custom-override s))

(define (abs n)
  (if (< n 0) (- n) n))

(define (in-list? n l)
  (cond
   ((null? l) #f)
   ((equal? n (car l)) #t)
   (else (in-list? n (cdr l)))))

(define (find n l)
  (cond
    ((null? l) #f)
    ((equal? n (car (car l))) (car l))
    (else (find n (cdr l)))))

(define (process-nutrients amount nutrients)
  (msg "amount is:" amount)
  (map
   (lambda (q)
     (if (number? q)
	 (rounding (* amount q))
	 q))
   nutrients))

(define (rounding a)
  (/ (round (* 10 a)) 10))
  
(define (get-qualities-for-type t)
  (cond
   ((eq? t 'cattle) cattle-quality-list)
   ((eq? t 'pig) pig-quality-list)
   ((eq? t 'poultry) poultry-quality-list)
   ((eq? t 'compost) compost-quality-list)
   (else fym-quality-list)))

(define (get-application-for-type t)
  (cond
   ((eq? t 'cattle) cattle-application-list)
   ((eq? t 'pig) pig-application-list)
   ((eq? t 'fym) fym-application-list)
   (else '())))

(define (get-units-for-type t)
  (cond
   ((eq? t 'cattle) "m3/ha")
   ((eq? t 'pig) "m3/ha")
   ((eq? t 'poultry) "tons/ha")
   ((eq? t 'compost) "tons/ha")
   (else "tons/ha")))

(define (imperial->metric amount units)
  (if (eq? (current-units) 'metric)
      amount
      (if (equal? units "m3/ha")
          (gallons/acre->m3/ha amount)
          (tons/acre->tons/ha amount))))

(define (metric->imperial amount units)
  (if (eq? (current-units) 'metric)
      amount
      (kg/ha->units/acre amount)))

(define (rounding-cash a)
  (/ (round (* 100 a)) 100))

(define (padcash->string a)
  (let ((t (number->string (rounding-cash a))))
    ;; removed in online version...
    ;;(substring t 0 (- (string-length t) 1))
    t))

(define (convert-input amount units)
  (if (eq? (current-units) 'metric)
      amount
      (cond
       ((or (equal? units "m3/ha") (equal? units "gallons/acre"))
        (gallons/acre->m3/ha amount))
       ((or (equal? units "tons/ha") (equal? units "tons/acre"))
        (tons/acre->tons/ha amount))
       ((or (equal? units "kg/ha") (equal? units "units/acre"))
        (units/acre->kg/ha amount))
       ((or (equal? units "hectares") (equal? units "acres"))
        (acres->hectares amount))
       ((equal? units "tonnes") amount) ;; tonnes are metric everywhere!?
       (else (msg "I don't understand how to convert" units)))))

(define (convert-output amount units)
  (rounding
   (if (eq? (current-units) 'metric)
       amount
       (cond
        ((or (equal? units "m3/ha") (equal? units "gallons/acre"))
         (m3/ha->gallons/acre amount))
        ((or (equal? units "tons/ha") (equal? units "tons/acre"))
         (tons/ha->tons/acre amount))
        ((or (equal? units "kg/ha") (equal? units "units/acre"))
         (kg/ha->units/acre amount))
        ((or (equal? units "hectares") (equal? units "acres"))
         (hectares->acres amount))
        ((or (equal? units "m3") (equal? units "gallons"))
         (m3->gallons amount))
        ((equal? units "tonnes") amount) ;; tonnes are metric everywhere!?
        (else (msg "I don't understand how to convert" units))))))

;; the manure calculator
(define calc
  (list 'pig 25 'DM2 'autumn 'normal 'mediumshallow 'splash-surface
	(list date-day date-month date-year)
	1 1 
	'(soil-p-0 soil-k-0)))

(define (calc-type s) (list-ref s 0))
(define (calc-modify-type s v) (list-replace s 0 v))
(define (calc-amount s) (list-ref s 1))
(define (calc-modify-amount s v) (list-replace s 1 v))
(define (calc-quality s) (list-ref s 2))
(define (calc-modify-quality s v) (list-replace s 2 v))
(define (calc-season s) (list-ref s 3))
(define (calc-modify-season s v) (list-replace s 3 v))
(define (calc-crop s) (list-ref s 4))
(define (calc-modify-crop s v) (list-replace s 4 v))
(define (calc-soil s) (list-ref s 5))
(define (calc-modify-soil s v) (list-replace s 5 v))
(define (calc-application s) (list-ref s 6))
(define (calc-modify-application s v) (list-replace s 6 v))
(define (calc-date s) (list-ref s 7))
(define (calc-modify-date s v) (list-replace s 7 v))
(define (calc-seek-mul s) (list-ref s 8))
(define (calc-modify-seek-mul s v) (list-replace s 8 v))
(define (calc-fieldsize s) (list-ref s 9))
(define (calc-modify-fieldsize s v) (list-replace s 9 v))
(define (calc-soil-test s) (list-ref s 10))
(define (calc-modify-soil-test s v) (list-replace s 10 v))

;; shortcuts
(define (current-date) (calc-date calc))
(define (current-seek-mul) (calc-seek-mul calc))
(define (current-type) (calc-type calc))
(define (current-quality) (calc-quality calc))

(define (calc-nutrients)
  (let ((type (calc-type calc))
	(amount (calc-amount calc))
	(quality (calc-quality calc))
	(season (calc-season calc))
	(crop (calc-crop calc))
	(soil (calc-soil calc))
	(application (calc-application calc))
	(soil-test (calc-soil-test calc)))
    (if custom-override  
	(process-nutrients amount custom-override)
	(get-nutrients type amount quality season crop soil application soil-test))))


(define (get-units)
  (let ((type (calc-type calc)))
    (if (eq? (current-units) 'metric)
        (get-units-for-type type)
        (if (equal? (get-units-for-type type) "m3/ha")
            "gallons/acre"
            "tons/acre"))))

(define (get-metric/imperial-units-for-type type)
  (if (eq? (current-units) 'metric)
      (get-units-for-type type)
      (if (equal? (get-units-for-type type) "m3/ha")
 	  "gallons/acre"
 	  "tons/acre")))

;; i18n??
(define (get-cost-string-from-nutrient nutrient-index amounts mul)
  (if (number? (list-ref amounts nutrient-index))  
      (string-append "£" (padcash->string (* (list-ref amounts nutrient-index)
					     (list-ref costs nutrient-index) mul)))
      "£N/A"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the complex bit

(define (previous-crop-grass? previous-crop)
  (or (eq? previous-crop 'grass-low-n)
      (eq? previous-crop 'grass-high-n)
      (eq? previous-crop 'grass-other)))

(define (is-crop-arable? c)
  (not (or (eq? c 'grass-cut) 
	   (eq? c 'grass-grazed))))

;; table on pp 188
(define (grassland-modifier crop soil previous-crop recently-grown-grass)
  ;; already checked it's grass -> grass
  ;; we don't know about previous arable crops
  (if (eq? recently-grown-grass 'yes)
      grassland-high-sns
      (if (eq? soil 'sandyshallow)
	  grassland-low-sns
	  grassland-med-sns)))

(define (sns-search tree params regularly-manure)
  (let ((sns (decision tree params)))
    ;; increase sns by one if they regularly manure (pp 188)
    (if (and (< sns 6) (eq? regularly-manure 'yes))
	(+ sns 1)
	sns)))
    
;; calculate soil nitrogen supply
(define (calc-sns rainfall soil crop previous-crop regularly-manure recently-grown-grass)
  (let ((params (list 
		 (list 'rainfall rainfall)
		 ;; soil medium category doesn't exist in sns tables
		 (list 'soil (if (eq? soil 'medium)
				 'mediumshallow soil))
		 (list 'previous-crop previous-crop))))
    ;; special options needed for grass:
    (cond
     ;; from grass -> grass (pp 188)
     ((and (previous-crop-grass? previous-crop)
	   (not (is-crop-arable? crop)))
      (grassland-modifier crop soil previous-crop recently-grown-grass))
     ;; grass -> arable (pp 94)
     ((and (is-crop-arable? crop) 
	   (previous-crop-grass? previous-crop))
      (sns-search previous-grass-soil-nitrogen-supply-tree
		  params regularly-manure))
     (else
      (sns-search soil-nitrogen-supply-tree 		  
		  params regularly-manure)))))


(define (get-crop-requirements/supply rainfall crop soil previous-crop regularly-manure soil-test-p soil-test-k recently-grown-grass)
  (let ((sns (calc-sns rainfall soil crop previous-crop regularly-manure recently-grown-grass)))
    (let ((choices 
	   (list 
	    (list 'sns sns) ;; sns not used for grass requirement, ok to be grassland low/med/high
	    (list 'rainfall rainfall)
	    (list 'soil soil)
	    (list 'crop crop)
	    (list 'p-index soil-test-p)
	    (list 'k-index soil-test-k))))
      (list 
       (+ (decision crop-requirements-n-tree choices) 
	  ;; add/subtract based on table on pp 188
	  (cond
	   ((eqv? sns grassland-high-sns) -30)
	   ((eqv? sns grassland-low-sns) 30)
	   (else 0)))
       (decision crop-requirements-pk-tree (cons (list 'nutrient 'phosphorous) choices))
       (decision crop-requirements-pk-tree (cons (list 'nutrient 'potassium) choices))
       sns))))


(define (get-nutrients type amount quality season crop soil application soil-test)
  ;; apply all the extra conditional stuff here
  (let ((params (list (list 'type type) 
		      (list 'quality quality) 
		      (list 'season season) 
		      ;; IMPORTANT: we have to convert crop from the 
		      ;; field types to send to manure
		      (list 'crop (cond
				   ((eq? crop 'grass-oilseed) 'grass-oilseed)
 				   ((eq? crop 'grass) 'grass-oilseed)
				   (else 'normal))) 
		      ;; also we have to convert soil to the two
		      ;; types in manure (pp 66, note b)
		      (list 'soil (cond 
				   ((eq? soil 'sandyshallow) 'sandyshallow)
				   ((eq? soil 'mediumshallow) 'sandyshallow)
				   (else 'mediumheavy)))
		      (list 'application application))))
    ;; get the total for pig or cattle slurry or poultry, then we apply the 
    ;; percent value later to get the crop available
    (let ((total (if (or (eq? type 'pig) 
			 (eq? type 'cattle)
			 (eq? type 'poultry))
		     (decision n-total-tree params) 0))
	  ;; high soil test means we're adding total
	  (phosphorous (if (or (eq? (car soil-test) 'soil-p-2) 
			       (eq? (car soil-test) 'soil-p-3))
			   'p-total
			   'p-avail))
	  (potassium (if (or (eq? (cadr soil-test) 'soil-k-2-)
			     (eq? (cadr soil-test) 'soil-k-2+) 
			     (eq? (cadr soil-test) 'soil-k-3))
			 'k-total
			 'k-avail)))

      (process-nutrients 
       amount 
       (list
	;; if pig or cattle slurry, then this is the percent value
	(let ((n (decision manure-tree (append (quote ((nutrient nitrogen))) params))))
	  ;; N/A value
	  (if (eq? n 'NA) n
	      ;; apply percent or return straight value
	      (if (zero? total) n (pc total n))))
	(decision manure-tree (append (list (list 'nutrient phosphorous)) params))
	(decision manure-tree (append (list (list 'nutrient potassium)) params)))))))
