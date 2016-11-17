;; calculator core, essentially a decision tree with a mutiplyer

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

(define costs (list 0.79 0.62 0.49))

(define images
  (list
   (list 'cattle
         (list
          (list 25 "cattle_25m3")
          (list 30 "cattle_30m3")
          (list 50 "cattle_50m3")
          (list 100 "cattle_100m3")))
   (list 'FYM
         (list
          (list 25 "fym_25t")
          (list 50 "fym_50t")))
   (list 'pig
         (list
          (list 25 "pig_25m3")
          (list 50 "pig_50m3")
          (list 75 "pig_75m3")))
   (list 'poultry
         (list
          (list 5 "poultry_5t")
          (list 10 "poultry_10t")))))

(define (abs n)
  (if (< n 0) (- n) n))

(define (find-image type amount)
  (define (_type images)
    (cond
     ((null? images) #f)
     ((equal? (car (car images)) type) (car images))
     (else (_type (cdr images)))))
  (define (_amount images s)
    (cond
     ((null? images) s)
     ((< (abs (- amount (car (car images))))
         (abs (- amount (car s))))
      (_amount (cdr images) (car images)))
     (else (_amount (cdr images) s))))
  (let ((type-images (_type images)))
    (if  (not type-images)
	 (begin
	   (msg "image not found for " type)
	   "cattle_25m3") ;; replace with blank image??
	 (cadr (_amount (cadr type-images) (car (cadr type-images)))))))


(define (nutrients type units amount table) (list type units amount table))
(define (nutrients-type n) (list-ref n 0))
(define (nutrients-units n) (list-ref n 1))
(define (nutrients-amount n) (list-ref n 2))
(define (nutrients-table n) (list-ref n 3))

(define (quality q n p k) (list q n p k))
(define (quality-q q) (list-ref q 0))
(define (quality-n q) (list-ref q 1))
(define (quality-p q) (list-ref q 2))
(define (quality-k q) (list-ref q 3))

;; nitrogen is based on season and crop
(define (nitrogen autumn winter spring summer)
  (list autumn winter spring summer))
(define (nitrogen-season n s)
  (cond
    ((equal? s 'autumn) (list-ref n 0))
    ((equal? s 'winter) (list-ref n 1))
    ((equal? s 'spring) (list-ref n 2))
    ((equal? s 'summer) (list-ref n 3))
    (else (error "season " (symbol->string s) " not found") #f)))

(define (soil sandyshallow mediumheavy)
  (list sandyshallow mediumheavy))
(define (soil? s) (list? s))
(define (get-soil s t)
  (cond
    ((equal? t 'sandyshallow) (list-ref s 0))
    ((equal? t 'mediumheavy) (list-ref s 1))
    (else 
     (error "soil type " (symbol->string t) " not found") 
     (list-ref s 0))))

(define (crop normal g)
  (list normal g))
(define (crop? c) (list? c))
(define (get-crop c t)
  (cond
    ((equal? t 'normal) (list-ref c 0))
    ((equal? t 'grass-oilseed) (list-ref c 1))
    (else 
     (error "crop type " (symbol->string t) " not found")
     (list-ref c 0))))

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

(define nutrients-metric
  (list
   (nutrients
    'cattle "m3/ha" 100
    (list
     (quality 'DM2 (nitrogen (soil (crop 8 16) (crop 48 56))  48 72 56) 30 220)
     (quality 'DM6 (nitrogen (soil (crop 13 26) (crop 65 78)) 65 91 65) 60 290)
     (quality 'DM10 (nitrogen (soil (crop 18 36) (crop 72 90)) 72 90 72) 90 360)))
   (nutrients
    'pig "m3/ha" 50
    (list
     (quality 'DM2 (nitrogen (soil (crop 15 22.5) (crop 52.5 60)) 60 82.5 82.5) 25 90)
     (quality 'DM4-pig (nitrogen (soil (crop 18 27) (crop 54 63)) 63 90 90) 45 110)
     (quality 'DM6-pig (nitrogen (soil (crop 22 33) (crop 55 66)) 66 99 99) 65 125)))
   (nutrients
    'poultry "tons/ha" 10
    (list
     (quality 'layer (nitrogen (soil (crop 19 28.5) (crop 47.5 57)) 47.5 66.5 66.5) 84 86)
     (quality 'broiler (nitrogen (soil (crop 30 45) (crop 75 90)) (soil 60 75) 90 90) 150 162)))
   (nutrients
    'compost "tons/ha" 10
    (list
     ;; nonsense values
     (quality 'green (nitrogen (soil (crop 19 28.5) (crop 47.5 57)) 47.5 66.5 66.5) 84 86)
     (quality 'green-food (nitrogen (soil (crop 30 45) (crop 75 90)) (soil 60 75) 90 90) 150 162)))
   (nutrients
    'FYM "tons/ha" 50
    (list
     (quality 'other (nitrogen (soil 15 30) 30 30 30) 95 360) ;; other
     (quality 'fresh (nitrogen (soil 15 30) 30 45 30) 95 360) ;; soil inc fresh
     (quality 'other1 (nitrogen (soil 15 30) 30 30 30) 95 360) ;; other
     (quality 'other2 (nitrogen (soil 15 30) 30 30 30) 95 360) ;; other
    ))))

(define (tons/acre->tons/ha a) (* a 2.47105381))
(define (tons/ha->tons/acre a) (/ a 2.47105381))
(define (gallons/acre->m3/ha a) (* a 0.0112336377))
(define (m3/ha->gallons/acre a) (/ a 0.0112336377))
(define (kg/ha->units/acre a) (* a 0.8))
(define (units/acre->kg/ha a) (/ a 0.8))
(define (acres->hectares a) (* a 0.404686))
(define (hectares->acres a) (* a 2.47105))
(define (m3->gallons a) (* a 219.969))

(define (error . args)
  (display (apply string-append args))(newline))

;; for demo purposes...
(define valid-manure-type-list (list 'cattle 'FYM 'pig 'poultry))

(define (coerce-type type)
  (if (not (in-list? type valid-manure-type-list))
      (begin
	(msg "coercing manure type")
	'cattle)
      type))

(define (coerce-quality type quality)
  (if (not (in-list? quality (get-qualities-for-type type)))
      (begin
	(msg "coercing quality")
	(car (get-qualities-for-type type)))
      quality))

(define (get-nutrients type amount quality season crop soil)
  (let ((type (coerce-type type))) ;; temp!!
    (let ((nutrients (find type nutrients-metric)))
      (if (not nutrients)
	  (begin 
	    (error "nutrients type not found for:" (symbol->string type))
	    (display nutrients)(newline))
	  (let ((q (find (coerce-quality type quality) (nutrients-table nutrients))))
	    (if (not q)
		(error "quality " (symbol->string (coerce-quality type quality)) " not found for " (symbol->string type))
		(get-nutrients-inner
		 (nutrients-amount nutrients)
		 (nutrients-units nutrients)
		 q amount season crop soil)))))))

(define (get-nutrients-inner quantity units quality amount season crop soil)
  (process-nutrients
   amount
   units
   quantity
   (list
    ;; nitrogen
    (let ((s (nitrogen-season (quality-n quality) season)))
      (if (not s)
          (error "season not found")
          (let ((c (if (soil? s)
                       (get-soil s soil)
                       s)))
            (if (crop? c)
                (get-crop c crop)
                c))))
    (quality-p quality)
    (quality-k quality))))

;; quantity is from the table (so I can debug easily it matches the data)
;; amount is from the slider
(define (process-nutrients amount units quantity nutrients)
  (map
   (lambda (q)
     (rounding (* amount (/ q quantity))))
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
  (let ((type (coerce-type (calc-type calc))))
    (if (eq? (current-units) 'metric)
        (nutrients-units (find type nutrients-metric))
        (if (equal? (nutrients-units (find type nutrients-metric)) "m3/ha")
            "gallons/acre"
            "tons/acre"))))

(define (nutrient-units event)
  (let ((type (event-type event)))
    (if (eq? (current-units) 'metric)
        (nutrients-units (find type nutrients-metric))
        (if (equal? (nutrients-units (find type nutrients-metric)) "m3/ha")
            "gallons/acre"
            "tons/acre"))))

(define (amount-units event)
  (let ((type (event-type event)))
    (if (eq? (current-units) 'metric)
        (if (equal? (nutrients-units (find type nutrients-metric)) "m3/ha")
            "m3" "tonnes")
        ;; it's imperial
        (if (equal? (nutrients-units (find type nutrients-metric)) "m3/ha")
            "gallons"
            "tonnes"))))

(define (get-cost-string-from-nutrient nutrient-index amounts mul)
  (padcash->string (* (list-ref amounts nutrient-index)
                      (list-ref costs nutrient-index) mul)))


;; geo stuff
(define earth_radius 6371009) ;; in meters
(define pi 3.14159265359)
(define (degrees->radians d) (* d 0.0174533))
(define lat_dist (* pi (/ earth_radius 180.0)))
(define (vx v) (list-ref v 0))
(define (vy v) (list-ref v 1))

(define (wrap-ref l i)
  (list-ref l (modulo i (length l))))

(define (reproject latlng)
  (list (* (vx latlng) lat_dist)
	(* (vy latlng) lat_dist 
	   (cos (degrees->radians (vx latlng))))))

(define (area-of-polygon points)
  (define (_ i r)
    (cond
     ((> i (- (length points) 1)) r)
     (else
      (_ (+ i 1)
	 (+ r (* (vx (wrap-ref points i))
		 (- (vy (wrap-ref points (+ i 1)))
		    (vy (wrap-ref points (- i 1))))))))))
  (abs (/ (_ -1 0) 2.0)))

;; not quite sure of this yet, needs checking properly
(define (recentre polygon)
  (map
   (lambda (p)
     (list (- (vx (car polygon)) (vx p))
           (- (vy (car polygon)) (vy p))))
   polygon))

(define (area-metres polygon)
  (area-of-polygon
   (recentre
    (map reproject polygon))))

(define (polygon-centroid polygon)
  (let ((t (foldl
            (lambda (latlng r)
              (list (+ (vx r) (vx latlng))
                    (+ (vy r) (vy latlng))))
            (list 0 0)
            polygon)))
    (if (zero? (length polygon))
	(list 0 0)
	(list (/ (vx t) (length polygon))
	      (/ (vy t) (length polygon))))))

(define (polygons-centroid polygons)
  (let ((t (foldl
	    (lambda (polygon r)     
	      (let ((latlng (polygon-centroid polygon)))
		(list (+ (vx r) (vx latlng))
		      (+ (vy r) (vy latlng)))))
	    (list 0 0)
	    polygons)))
    (if (zero? (length polygons))
	(list 0 0)
	(list (/ (vx t) (length polygons))
	      (/ (vy t) (length polygons))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit tests

(define (assert id p v)
  (when (not (eq? p v)) (error "test" id "failed" p v)))

(define (test)
  (assert 1 (get-param-value '((one 1) (two 2)) 'two) 2)
  (assert 2 (get-param-value '((one 1) (two 2)) 'three) #f)
  (assert 3 (dtree-choose '(name ((one 1) (two 2) 77)) 'two) 2) 
  (assert 4 (dtree-choose '(name ((one 1) (two 2) 77)) 'three) 77) 
  (assert 5 (decision '(name ((one 1)
                              (two (season
                                    ((winter 3)
                                     39)))
                              77))
                      '((name two)
                        (season spring))) 39)
  (assert 6 (decision '(name ((one 1)
                              (two (season
                                    ((winter 3)
                                     39)))
                              77))
                      '((name two)
                        (season winter))) 3)
  )

(test)

