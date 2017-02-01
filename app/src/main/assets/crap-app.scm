;; Farm Crap App Pro Copyright (C) 2016 Dave Griffiths
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define db "/sdcard/farmcrapapppro/crapapp.db")
(set-current! 'db db)

(define (setup-database!)
  (db-close db) ;; close just in case (sorts out db file delete while running problem)
  (db-open db)
  (setup db "local")
  (setup db "farm")
  (insert-entity-if-not-exists
   db "local" "app-settings" "null" settings-entity-id-version
   (list
    (ktv "user-id" "varchar" "None Yet")
    (ktv "language" "int" 0)
    (ktv "email" "varchar" "None Yet")
    (ktv "units" "varchar" "metric")
    (ktv "rainfall" "varchar" "medium")
    (ktv "cost-n" "real" 0.79)
    (ktv "cost-p" "real" 0.62)
    (ktv "cost-k" "real" 0.49))))

(define (nuke-database!)
  (nuke db "local")
  (nuke db "farm")
  (setup-database!))
  
;; stuff here depends on the android update mechanism

(define (get-custom-types)
  (map
   (lambda (e)
     (ktv-get e "name" ))
   (db-filter-only 
    db "farm" "manure" 
    (list)
    (list (list "name" "varchar")))))

(define (get-custom-type name)
  (map
   (lambda (e)
     (list (ktv-get e "N") (ktv-get e "P") (ktv-get e "K")))
   (db-filter-only 
    db "farm" "manure" 
    (list (list "name" "varchar" "=" name))
    (list (list "N" "real") (list "P" "real") (list "K" "real")))))

(define (get-qualities-for-type-inc-custom type)
  (if (eq? type 'custom-manure)
      (get-custom-types)
      (get-qualities-for-type type)))

(define (update-calc! fn)
  (set! calc (fn calc))
  (run-calc))

(define (update-type! v) 
  (update-calc! 
   (lambda (c) 
     (calc-modify-quality ;; need to set a valid quality for this type
      (calc-modify-type c v)
      (car (get-qualities-for-type-inc-custom v)))))) ;; argh - sets to first

(define (update-amount! v) (update-calc! (lambda (c) (calc-modify-amount c v))))
(define (update-quality! v) (update-calc! (lambda (c) (calc-modify-quality c v))))
(define (update-application! v) (update-calc! (lambda (c) (calc-modify-application c v))))
(define (update-season! v) (update-calc! (lambda (c) (calc-modify-season c v))))
(define (update-crop! v) (update-calc! (lambda (c) (calc-modify-crop c v))))
(define (update-soil! v) (update-calc! (lambda (c) (calc-modify-soil c v))))
(define (update-fieldsize! v) (update-calc! (lambda (c) (calc-modify-fieldsize c v))))
(define (update-soil-test! v) (update-calc! (lambda (c) (calc-modify-soil-test c v))))
(define (clear-soil-test!) (update-calc! (lambda (c) (calc-modify-soil-test c '(soil-p-0 soil-k-0)))))

;; locally stored in the database
(define (mutate-units! v) (set-setting! "units" "varchar" (symbol->string v)))
(define (current-units) (string->symbol (get-setting-value "units")))
 
(define (mutate-email! v) (set-setting! "email" "varchar" v))
(define (current-email) (get-setting-value "email"))

(define (mutate-rainfall! v) (set-setting! "rainfall" "varchar" (symbol->string v)))
(define (current-rainfall) (string->symbol (get-setting-value "rainfall")))

(define (mutate-cost-n! v) (set-setting! "cost-n" "real" (safe-string->number v)) (update-costs))
(define (current-cost-n) (get-setting-value "cost-n"))
(define (mutate-cost-p! v) (set-setting! "cost-p" "real" (safe-string->number v)) (update-costs))
(define (current-cost-p) (get-setting-value "cost-p"))
(define (mutate-cost-k! v) (set-setting! "cost-k" "real" (safe-string->number v)) (update-costs))
(define (current-cost-k) (get-setting-value "cost-k"))

(define (update-costs)
  (set! costs (list (current-cost-n)
		    (current-cost-p)
		    (current-cost-k))))

(define (mutate-current-seek-mul! a)
  (set! calc (calc-modify-seek-mul calc a)))

(define (run-calc)
  (let ((amounts (calc-nutrients))
        (amount (calc-amount calc))
        (type (calc-type calc))
	(size (calc-fieldsize calc)))
    (append
     (list
      (update-widget 'text-view (get-id "amount-value") 'text
		     (string-append (number->string (convert-output amount (get-units))) " " (get-units)))
      (update-widget 'text-view (get-id "na")
		     'text 
		     (if (eq? (list-ref amounts 0) 'NA)
			 "N/A"
			 (number->string (convert-output (list-ref amounts 0) "kg/ha"))))
      (update-widget 'text-view (get-id "pa")
		     'text (number->string (convert-output (list-ref amounts 1) "kg/ha")))
      (update-widget 'text-view (get-id "ka")
		     'text (number->string (convert-output (list-ref amounts 2) "kg/ha")))
      ;; costs
      (update-widget 'text-view (get-id "costn")
		     'text (get-cost-string-from-nutrient 0 amounts size))
      (update-widget 'text-view (get-id "costp")
		     'text (get-cost-string-from-nutrient 1 amounts size))
      (update-widget 'text-view (get-id "costk")
		     'text (get-cost-string-from-nutrient 2 amounts size)))
     
     ;; still needed
     (if (eq? (get-current 'calc-mode #f) 'fieldcalc)
	 (begin
	   (list
	    (update-widget 'text-view (get-id "needed-n")
			   'text 
			   (if (eq? (list-ref amounts 0) 'NA)
			       "N/A"
			       (number->string (convert-output (- (entity-get-value "require-n") (list-ref amounts 0)) "kg/ha"))))
	    (update-widget 'text-view (get-id "needed-p")
			   'text (number->string (convert-output (- (entity-get-value "require-p") (list-ref amounts 1)) "kg/ha")))
	    (update-widget 'text-view (get-id "needed-k")
			   'text (number->string (convert-output (- (entity-get-value "require-k") (list-ref amounts 2)) "kg/ha")))
	    )) '())
     )))


(define (spacer size)
  (space (layout 'fill-parent size 1 'left 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maximum a b)
  (if (> a b) a b))

(define graph-width 320)
(define graph-n-col (map (lambda (x) (maximum 0 (- x 20))) '(0 255 107)))
(define graph-p-col (map (lambda (x) (maximum 0 (- x 20))) '(238 239 119)))
(define graph-k-col (map (lambda (x) (maximum 0 (- x 20))) '(255 0 249)))

(define (build-lines events min max colour n)
  (let ((twidth (maximum (- max min) 1)))
    (cadr (foldl
           (lambda (event r)
             (let* ((t (date->day (ktv-get event "date")))
                    (last-point (car r))
                    (points-list (cadr r))
                    (x (* graph-width (/ (- t min) twidth)))
                    (y (- 250 (list-ref (event-nutrients event) n))))
               (list
                (list x y)
                (cons (drawlist-line
                       colour 5 (list (car last-point) (cadr last-point)
                                      x y))
                      points-list))))
           (list (list -10 250) '())
           events))))

(define (build-bars events min max)
  (let* ((twidth (maximum (- max min) 1))
         (month-width (* (/ 30 twidth) graph-width)))
    (foldl
     (lambda (event r)
       (let* ((t (date->day (ktv-get event "date")))
              (x (* graph-width (/ (- t min) twidth)))
              (y1 (- 250 (ktv-get event "nutrients-n")))
	      (y2 (- 250 (ktv-get event "nutrients-p")))
	      (y3 (- 250 (ktv-get event "nutrients-k"))))
	 (append
	  (if (< month-width 20)
	      (list
	       (drawlist-line graph-n-col 3 (list x 250 x y1))
	       (drawlist-line graph-p-col 3 (list (+ x 3) 250 (+ x 3) y2))
	       (drawlist-line graph-k-col 3 (list (+ x 6) 250 (+ x 6) y3)))
	      (list
	       (drawlist-line graph-n-col 10 (list x 250 x y1))
	       (drawlist-line graph-p-col 10 (list (+ x 5) 250 (+ x 5) y2))
	       (drawlist-line graph-k-col 10 (list (+ x 10) 250 (+ x 10) y3))))
	  r)))
     '()
     events)))

(define (month->text m)
  (cond
   ((eqv? m 1) "January")
   ((eqv? m 2) "Febuary")
   ((eqv? m 3) "March")
   ((eqv? m 4) "April")
   ((eqv? m 5) "May")
   ((eqv? m 6) "June")
   ((eqv? m 7) "July")
   ((eqv? m 8) "August")
   ((eqv? m 9) "September")
   ((eqv? m 10) "October")
   ((eqv? m 11) "November")
   ((eqv? m 12) "December")))

(define (build-t-scale first min max)
  (define (_y year-width x y)
    (if (> (- x (/ year-width 2)) graph-width)
        '()
        (append
         (list
          (drawlist-text (number->string y)
                         (- x (/ year-width 2))
                         180 '(150 150 150) 20 "vertical")
          (drawlist-line '(0 0 0) 1
                         (list x 0 x 250)))
         (_y year-width (+ x year-width) (+ y 1)))))

  (define (_m month-width x m)
    (if (> (- x (/ month-width 2)) graph-width)
        '()
        (append
         (list
          (drawlist-text (month->text (+ m 1))
                         (- x (/ month-width 2))
                         180 '(0 0 0) 20 "vertical")
          (drawlist-line '(0 0 0) 1
                         (list x 0 x 250)))
         (_m month-width (+ x month-width) (modulo (+ m 1) 12)))))

  (let* ((twidth (maximum (- max min) 1))
         (month-width (* (/ 30 twidth) graph-width))
         (first-month-x (* (/ (- 30 (list-ref first 0)) twidth) graph-width)))
    (if (<= twidth 0)
        '()
        (if (> month-width 20)
            (_m month-width first-month-x (- (list-ref first 1) 1))
            (_y
             (* (/ 365 twidth) graph-width)
             (* (/ (- 12 (list-ref first 1)) twidth) graph-width)
             (list-ref first 2))))))

(define (build-key)
  (let ((units (if (eq? (current-units) 'metric)
                   "Kg/hectare"
                   "units/acre"))
        (a (if (equal? (current-units) 'metric) 100 (convert-output 200 "kg/ha")))
        (b (if (equal? (current-units) 'metric) 150 (convert-output 150 "kg/ha")))
        (c (if (equal? (current-units) 'metric) 100 (convert-output 100 "kg/ha")))
        (d (if (equal? (current-units) 'metric) 50 (convert-output 50 "kg/ha"))))
    (list
     (drawlist-text units 15 180 '(0 0 0) 15 "vertical")
     (drawlist-text a 20 50 '(0 0 0) 10 "horizontal")
     (drawlist-text b 20 100 '(0 0 0) 10 "horizontal")
     (drawlist-text c 20 150 '(0 0 0) 10 "horizontal")
     (drawlist-text d 20 200 '(0 0 0) 10 "horizontal")
     (drawlist-text "N" 280 30 graph-n-col 20 "horizontal")
     (drawlist-text "P" 280 60 graph-p-col 20 "horizontal")
     (drawlist-text "K" 280 90 graph-k-col 20 "horizontal")
     )))

(define (newest-event-day events)
  (foldl
   (lambda (event latest)
     (let ((d (date->day (ktv-get event "date"))))
       (if (< latest d)
	   d latest)))
   0
   events))

(define (oldest-event events)
  (cadr
   (foldl
    (lambda (event latest)
      (let ((d (date->day (ktv-get event "date"))))
	(if (< d (car latest))
	    (list d event) latest)))
    (list 9999999 '())
    events)))


(define (oldest-event-day events)
  (foldl
   (lambda (event latest)
     (let ((d (date->day (ktv-get event "date"))))
       (if (< d latest)
	   d latest)))
   999999
   events))

(define (build-graph)
  (let ((events (db-filter 
		 db "farm" "event" 
		 (list
		  (list "parent" "varchar" "=" (get-current 'field-id #f))))))
    (append
     (if (> (length events) 1)
	 (let* ((_min (oldest-event-day events))
		(_max (newest-event-day events))
                (safe (* (- _max _min) 0.1))
                (min (- _min safe))
                (max (+ _max safe)))
           (append
            (build-t-scale (string->date (ktv-get (oldest-event events) "date")) min max)
            (build-bars events min max)
            (build-key)))
         (list (drawlist-text "Not enough events for graph"
                              20 105 '(0 0 0) 20 "horizontal")))
     (list
      (drawlist-line '(0 0 0) 5 (list 0 0 320 0))
      (drawlist-line '(0 0 0) 5 (list 0 250 320 250))))))

(define (setup-for-picture-from-event)
  ;; setup the calculator values for the camera pic from the event
  (mutate-state!
   (lambda (s)
     (state-modify-calc
      s (calc-modify-amount
         (calc-modify-type
          (state-calc s)
          (event-type (current-event)))
         (event-amount (current-event)))))))

(define (update-seek-mul! manure)
  (if (and (eq? (current-units) 'imperial)
           (or (eq? manure 'cattle)
               (eq? manure 'pig)))
      (mutate-current-seek-mul! 100)
      (cond
       ((eq? manure 'poultry)
        (if (eq? (current-units) 'imperial)
            (mutate-current-seek-mul! 0.1)
            (mutate-current-seek-mul! 0.15)))
       (else
        (mutate-current-seek-mul! 1)))))

;; just for graph so don't have to be accurate!!!
(define (date->day d)
  (let ((d (string->date d)))
    (+ (* (list-ref d 2) 360)
       (* (list-ref d 1) 30)
       (list-ref d 0))))  

(define (date< a b)
  (cond
   ((< (list-ref a 2) (list-ref b 2)) #t)
   ((> (list-ref a 2) (list-ref b 2)) #f)
   (else ;; year is the same
    (cond
     ((< (list-ref a 1) (list-ref b 1)) #t)
     ((> (list-ref a 1) (list-ref b 1)) #f)
     (else ;; month is the same
      (cond
       ((< (list-ref a 0) (list-ref b 0)) #t)
       ((> (list-ref a 0) (list-ref b 0)) #f)
       (else #f)))))))


(define (string->date d)
  (let ((splot (string-split d (list #\/))))
    (list 
     (string->number (list-ref splot 0))
     (string->number (list-ref splot 1))
     (string->number (list-ref splot 2)))))

(define (date->string d)
  (string-append
   (number->string (list-ref d 0))
   "/"
   (number->string (list-ref d 1))
   "/"
   (number->string (list-ref d 2))))

(define (date->path d)
  (string-append
   (number->string (list-ref d 0))
   "-"
   (number->string (list-ref d 1))
   "-"
   (number->string (list-ref d 2))))

(define (date->season d)
  (cond
   ((or
     (eqv? (list-ref d 1) 2)
     (eqv? (list-ref d 1) 3)
     (eqv? (list-ref d 1) 4)) 'spring)
   ((or
     (eqv? (list-ref d 1) 5)
     (eqv? (list-ref d 1) 6)
     (eqv? (list-ref d 1) 7)) 'summer)
   ((or
     (eqv? (list-ref d 1) 8)
     (eqv? (list-ref d 1) 9)
     (eqv? (list-ref d 1) 10)) 'autumn)
   ((or
     (eqv? (list-ref d 1) 11)
     (eqv? (list-ref d 1) 12)
     (eqv? (list-ref d 1) 1)) 'winter)))


(define (get-polygons)
  (map
   (lambda (field)
     (list
      (ktv-get field "name")
      (ktv-get field "unique_id")
      ;; info text
      (list 
       (mtext-lookup (string->symbol (ktv-get field "crop")))
       (string-append 
	(number->string (convert-output (ktv-get field "size") "hectares")) 
	(if (eq? (current-units) 'imperial) "acres" "ha")))
      (get-field-polygon (ktv-get field "unique_id"))))
   (db-all db "farm" "field")))

(define (get-field-polygon field-uid)
  (map
   (lambda (coord)
     (list
      (ktv-get coord "lat") 
      (ktv-get coord "lng")))
   (db-filter 
    db "farm" "coord" 
    (list
     (list "parent" "varchar" "=" field-uid)))))  

(define (get-farm-centre field-polygons)
  (polygons-centroid
   (map (lambda (field)
	  (list-ref field 3))
	field-polygons)))

(define (get-field-centre field-uid polygons)
  (let ((r (polygon-centroid (get-field-polygon field-uid))))
    ;; default to farm centre if there is no field poly yet
    (if (and (eqv? (car r) 0) (eqv? (cadr r) 0))
	(get-farm-centre polygons)
	r)))

(define (polygons-empty? p)
  (or (null? p)
      (null? (list-ref (car p) 3))))

(define (event-view-item id title bg)
  (linear-layout
   0
   'horizontal 
   (layout 'fill-parent 'wrap-content 1 'centre 5)
   (if bg list-colour (list 0 0 0 0))
   (list
    (text-view (make-id (string-append id "-text")) (mtext-lookup title) 20 (layout 'fill-parent 'wrap-content 1 'centre 0))
    (text-view (make-id id) "" 30 (layout 'fill-parent 'wrap-content 1 'centre 0)))))

(define (update-event-view-item id)
  (cond
   ((equal? id "size")
    (update-widget 'text-view (get-id id) 'text (convert-output (entity-get-value id) "hectares")))
   ((equal? id "amount")
    (update-widget 'text-view (get-id id) 'text (convert-output (entity-get-value id) (get-units-for-type (string->symbol (entity-get-value "type"))))))
   (else
    (update-widget 'text-view (get-id id) 'text (entity-get-value id)))))

(define (update-event-view-item-lookup id)
  (update-widget 'text-view (get-id id) 'text (mtext-lookup (string->symbol (entity-get-value id)))))

(define (calc-results)
  (vert
   (mtext 'crop-availible)
   (horiz
    (mtext-scale 'nutrient-n-metric)
    (mtext-scale 'nutrient-p-metric)
    (mtext-scale 'nutrient-k-metric))
   (horiz
    (text-view (make-id "na") "12" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
    (text-view (make-id "pa") "12" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
    (text-view (make-id "ka") "12" 30 (layout 'wrap-content 'wrap-content 1 'centre 0)))
   
   (mtext 'cost-saving)
   (horiz
    (text-view (make-id "costn") "12" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
    (text-view (make-id "costp") "12" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
    (text-view (make-id "costk") "12" 30 (layout 'wrap-content 'wrap-content 1 'centre 0)))
   (spacer 10)
   (image-view (make-id "example") "test" (layout 'fill-parent 'fill-parent 1 'centre 0))
   (spacer 10)))

(define (calc-event-results)
  (vert
   (mtext 'crop-availible)
   (horiz
    (mtext-scale 'nutrient-n-metric)
    (mtext-scale 'nutrient-p-metric)
    (mtext-scale 'nutrient-k-metric))
   (horiz
    (text-view (make-id "na") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
    (text-view (make-id "pa") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
    (text-view (make-id "ka") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0)))
   
   (mtext 'cost-saving)
   (horiz
    (text-view (make-id "costn") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
    (text-view (make-id "costp") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
    (text-view (make-id "costk") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0)))

   (mtext 'crop-requirements)
   (horiz
    (text-view (make-id "require-n") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
    (text-view (make-id "require-p") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
    (text-view (make-id "require-k") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0)))

   (mtext 'still-needed)
   (horiz
    (text-view (make-id "needed-n") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
    (text-view (make-id "needed-p") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
    (text-view (make-id "needed-k") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0)))

   (spacer 10)
   (image-view (make-id "example") "test" (layout 'fill-parent 'fill-parent 1 'centre 0))
   (spacer 10)))

(define (calc-manure-type-widget fn)
  (mspinner 
   'manure-type manure-type-list
   (lambda (v) 
     (let ((v (list-ref manure-type-list v)))
       (update-seek-mul! v)
       (fn v)
       (append
	(update-type! v)
	(update-amount! (convert-input (* (current-seek-mul) 50) (get-units)))
	(list
	 (update-widget 'seek-bar (get-id "amount") 'init 0)
	 (update-widget 'spinner (get-id "quality-spinner") 'array
			(symbol-list-to-names
			 (get-qualities-for-type-inc-custom v)))
	 (update-widget 'image-view (get-id "example") 'image
			(find-image (calc-type calc)
				    (calc-amount calc)))
	 )
	(let ((applications (get-application-for-type v)))
	  (if (null? applications)
	      (list
	       (update-widget 'spinner (get-id "application-type-spinner") 'disabled 0)
	       (update-widget 'spinner (get-id "application-type-spinner") 'array '("None")))
	      (list
	       (update-widget 'spinner (get-id "application-type-spinner") 'enabled 0)
	       (update-widget 'spinner (get-id "application-type-spinner") 'array
			      (symbol-list-to-names applications)))))	   
	)))))

(define (calc-manure-quality-widget fn)
  (mspinner 'quality cattle-quality-list 
	    (lambda (v) 
	      (let ((type (current-type)))
		(cond
		 ((eq? type 'custom-manure)
		  (let ((name (list-ref (get-custom-types) v)))
		    (let ((custom (get-custom-type name)))
		      (when (> (length custom) 0)
			    (set-custom-override! (car custom)))		      
		      (fn name) '())))
		 (else
		  (set-custom-override! #f)
		  (let ((quality 
			 (list-ref 
			  (cond
			   ((eq? type 'cattle) cattle-quality-list)
			   ((eq? type 'pig) pig-quality-list)
			   ((eq? type 'poultry) poultry-quality-list)
			   (else fym-quality-list))
			  v)))
		    
		    (fn quality)
		    (update-quality! quality))))))))

(define (calc-manure-application-widget fn)
  (mspinner 'application-type cattle-application-list 
	    (lambda (v) 
	      (let ((type (current-type)))
		(let ((application-list (get-application-for-type type)))
		  (let ((application (if (null? application-list) 'none
					 (list-ref application-list v))))
		    (fn application)
		    (update-application! application)))))))

(define (calc-amount-widget fn)
  (seek-bar (make-id "amount") 100 fillwrap
	    (lambda (v)
	      ;; make it match the display!
	      (let ((v (rounding v)))
		(fn v)
		(append
		 (update-amount! (convert-input (* (current-seek-mul) v) (get-units)))
		 (list
		  (update-widget 'image-view (get-id "example") 'image
				 (find-image (calc-type calc)
					     (calc-amount calc)))))))))
  
(define (photo-path)
  (string-append
   (get-current 'field-name "??") 
   "-"
   (date->path (string->date (entity-get-value "date")))
   "/"))

(define (calc-gallery)
  (linear-layout
   (make-id "gallery")
   'vertical
   (layout 'fill-parent 'wrap-content 1 'centre 20)
   list-colour
   (list
    (mbutton 'load-gallery
	     (lambda ()
	      (let ((path (photo-path)))
		(list
		 (list-files
		  (string-append "filelister-" path)
		  path
		  (lambda (images)
		    (list
		     (update-widget
		      'linear-layout (get-id "gallery") 'contents
		      (cons
		       (mtitle 'gallery)
		       (foldl
			(lambda (image r)
			  (append
			   (list 
			    (image-button (make-id image)
					  (string-append dirname path image)
					  (layout 'wrap-content 'wrap-content 1 'centre 0)
					  (lambda () (list 
						      (view (string-append dirname path image)))))
			    (spacer 10))
			   r))
			'()
			images)))))))))))))


(define (update-field-cropsoil-calc-from-current)
  (update-field-cropsoil-calc
   (get-crop-requirements/supply-from-current)))

(define (update-text-view-units id metric imperial)
  (update-widget 
   'text-view 
   (symbol->id id) 
   'text 
   (mtext-lookup (if (eq? (current-units) 'imperial) 
		     imperial metric))))

(define (update-field-cropsoil-calc results)
  (list
   (update-widget 'text-view (get-id "supply-n") 'text 
		  (if (eq? (current-units) 'imperial)
		      (soil-nutrient-code-to-text-imperial (list-ref results 3))
		      (soil-nutrient-code-to-text (list-ref results 3))))
   (update-widget 'text-view (get-id "require-n") 'text (number->string (convert-output (list-ref results 0) "kg/ha")))
   (update-widget 'text-view (get-id "require-p") 'text (number->string (convert-output (list-ref results 1) "kg/ha")))
   (update-widget 'text-view (get-id "require-k") 'text (number->string (convert-output (list-ref results 2) "kg/ha")))))

(define (get-crop-requirements/supply-from-field field)
  (get-crop-requirements/supply 
   (current-rainfall)
   (string->symbol (ktv-get field "crop"))
   (string->symbol (ktv-get field "soil"))
   (string->symbol (ktv-get field "previous-crop"))
   (string->symbol (ktv-get field "regularly-manure"))
   (string->symbol (ktv-get field "soil-test-p"))
   (string->symbol (ktv-get field "soil-test-k"))
   (string->symbol (ktv-get field "recently-grown-grass"))))

(define (get-crop-requirements/supply-from-current)
  (get-crop-requirements/supply 
   (current-rainfall)
   (string->symbol (entity-get-value "crop"))
   (string->symbol (entity-get-value "soil"))
   (string->symbol (entity-get-value "previous-crop"))
   (string->symbol (entity-get-value "regularly-manure"))
   (string->symbol (entity-get-value "soil-test-p"))
   (string->symbol (entity-get-value "soil-test-k"))
   (string->symbol (entity-get-value "recently-grown-grass"))))

(define (save-data filename d)
  (let ((f (open-output-file (string-append dirname filename))))
    (display d f)
    (close-output-port f))
  d)


(define (crap-titles)
  (string-append  
   "Field name,"
   "Manure type,"
   "Date," 
   "Crop avail N,"
   "Crop avail P,"
   "Crop avail K,"
   "Crop require N,"
   "Crop require P,"
   "Crop require K,"
   "SNS," 
   "Soil,"
   "Field size,"
   "Rate,"
   "Manure quality," 
   "Manure application,"
   "Season," 
   "Crop"))

(define (crap-csv db table entity-type)
  (let ((manure-type 'cattle)
	(s (db-filter-only 
	    db "farm" "event" 
	    (list)
	    (list 
	     '("parent" "varchar")
	     '("type" "varchar")
	     '("date" "varchar")
	     '("nutrients-n" "real")
	     '("nutrients-p" "real")
	     '("nutrients-k" "real")
	     '("require-n" "real")
	     '("require-p" "real")
	     '("require-k" "real")
	     '("sns" "int")
	     '("soil" "varchar")
	     '("size" "real")
	     '("amount" "real")
	     '("quality" "varchar")
	     '("application" "varchar")
	     '("season" "varchar")
	     '("crop" "varchar")))))
    
    (if (null? s)
	;; nothing here, just return titles
	(crap-titles)
	(foldl
	 (lambda (entity r)
	   (let ((entity (reverse entity)))
	     (string-append
	      r "\n"
	      (foldl
	       (lambda (ktv r)
		 (cond
		  ((equal? (ktv-key ktv) "unique_id") r)
		  ((equal? (ktv-key ktv) "parent") 
		   (string-append r "\"" (get-entity-name db table (ktv-value ktv)) "\""))
		  ((null? (ktv-value ktv))
		   (msg "value not found in csv for " (ktv-key ktv))
		   (string-append r ", NULL"))

		  ((equal? (ktv-key ktv) "date")
		   (string-append r ", \"" (ktv-value ktv) "\""))		  
	
		  ((equal? (ktv-key ktv) "type")
		   (set! manure-type (string->symbol (ktv-value ktv)))
		   (string-append r ", \"" (mtext-lookup manure-type) "\""))
		  
		  ;; look up translated
		  ((in-list? (ktv-key ktv)
			     (list "soil" "application" 
				   "season" "crop"))		   
		   (string-append r ", \"" (mtext-lookup (string->symbol (ktv-value ktv))) "\""))
		  
		  ((equal? (ktv-key ktv) "quality")		   
		   (if (eq? manure-type 'custom-manure)
 		       (string-append r ", \"" (ktv-value ktv) "\"")
		       (string-append r ", \"" (mtext-lookup (string->symbol (ktv-value ktv))) "\"")))	 
	  
		  ;; convert value
		  ((in-list? (ktv-key ktv)
			     (list "nutrients-n" "nutrients-p" "nutrients-k" 
				   "require-n" "require-p" "require-k"))		   
		   (string-append r ", \"" (number->string (convert-output (ktv-value ktv) "kg/ha")) "\""))
		  
		  ((equal? (ktv-key ktv) "sns")
		   (string-append 
		    r ", \"" 
		    (if (eq? (current-units) 'imperial)
			(soil-nutrient-code-to-ascii-imperial (ktv-value ktv))
			(soil-nutrient-code-to-ascii (ktv-value ktv)))
		    "\""))

		  ((equal? (ktv-key ktv) "size")
		   (string-append r ", \"" (number->string (convert-output (ktv-value ktv) "hectares")) "\""))
		  
		  ((equal? (ktv-key ktv) "amount")
		   (string-append 
		    r ", \"" (number->string 
			      (convert-output 
			       (ktv-value ktv) 
			       (get-metric/imperial-units-for-type manure-type)))
		    "\"")) 
		  
		  (else
		   (string-append r ", \"" (stringify-value-url ktv) "\""))))
	       ""
	       entity))))
	 (crap-titles)
	 s))))
