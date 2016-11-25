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

(define manure-type-list (list 'cattle 'FYM 'pig 'poultry 'compost 'custom-manure))
(define units-list (list 'metric 'imperial))
(define soil-type-list 
  (list 'sandyshallow 
	'mediumheavy ;; remove 
	'peat 'organic 'mediumshallow 'deepclay 'deepsilt))
(define soil-test-n-list (list 'soil-n-0 'soil-n-1 'soil-n-2 'soil-n-3))
(define soil-test-p-list (list 'soil-p-0 'soil-p-1 'soil-p-2- 'soil-p-2+ 'soil-p-3))
(define crop-type-list 
  (list 'normal 'grass-oilseed  ;; remove these
	'winter-wheat-removed 'winter-wheat-incorporated 
	'spring-barley-removed 'spring-barley-incorporated 'grass))
(define previous-crop-type-list 
  (list 'cereals 'oilseed 'potatoes 
	'sugarbeet 'peas 'beans 'low-n-veg 'medium-n-veg 'forage 
	'uncropped 'grass-low-n 'grass-high-n 'grass-other))
(define season-list (list 'autumn 'winter 'spring 'summer))
(define cattle-quality-list (list 'DM2 'DM6 'DM10))
(define pig-quality-list (list 'DM2 'DM4-pig 'DM6-pig))
(define poultry-quality-list (list 'layer 'broiler))
(define fym-quality-list (list 'fresh 'other 'other1 'other2))
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

;; remove...
(define costs (list 0.79 0.62 0.49))

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

(define (process-nutrients amount units quantity nutrients)
  (map
   (lambda (q)
     (rounding (* amount (/ q quantity))))
   nutrients))

(define (rounding a)
  (/ (round (* 10 a)) 10))

(define (get-nutrients type amount quality season crop soil)
  (let ((params (list (list 'type type) (list 'quality quality) (list 'season season) (list 'crop crop) (list 'soil soil))))
    (list
     (decision tree (append '((nutrient nitrogen)) params))
     (decision tree (append '((nutrient phosphorous)) params))
     (decision tree (append '((nutrient potassium)) params)))))

(define (error . args)
  (display (apply string-append args))(newline))

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

(define (get-multiplier-for-type t)
   ((eq? t 'cattle) 100)
   ((eq? t 'pig) 50)
   ((eq? t 'poultry) 10)
   ((eq? t 'compost) 10)
   (else 50))

(define (get-units-for-type t)
   ((eq? t 'cattle) "m3/ha")
   ((eq? t 'pig) "m3/ha")
   ((eq? t 'poultry) "tons/ha")
   ((eq? t 'compost) "tons/ha")
   (else "tons/ha"))

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
  (let ((t (number->string (+ (rounding-cash a) 0.001))))
    (substring t 0 (- (string-length t) 1))))

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

;; the calculator
(define calc
  (list 'pig 25 'DM2 'autumn 'normal 'mediumheavy
	(list date-day date-month date-year)
	1))

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
(define (calc-date s) (list-ref s 6))
(define (calc-modify-date s v) (list-replace s 6 v))
(define (calc-seek-mul s) (list-ref s 7))
(define (calc-modify-seek-mul s v) (list-replace s 7 v))

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
	(soil (calc-soil calc)))
    (get-nutrients type amount quality season crop soil)))

(define (get-units)
  (let ((type (calc-type calc)))
    (if (eq? (current-units) 'metric)
        (get-units-for-type type)
        (if (equal? (get-units-for-type type) "m3/ha")
            "gallons/acre"
            "tons/acre"))))

(define (get-cost-string-from-nutrient nutrient-index amounts mul)
  (padcash->string (* (list-ref amounts nutrient-index)
                      (list-ref costs nutrient-index) mul)))


