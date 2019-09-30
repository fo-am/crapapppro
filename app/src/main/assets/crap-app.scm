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

(msg "crap-app")

(define db "/sdcard/farmcrapapppro-beta/crapapp.db")
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
    (ktv "current-farm" "varchar" "none")
    (ktv "backup-freq" "varchar" "never")
    (ktv "last-backup" "varchar" "")

;; removed and put in farm entity
;;    (ktv "rainfall" "varchar" "medium")
;;    (ktv "cost-n" "real" 0.79)
;;    (ktv "cost-p" "real" 0.62)
;;    (ktv "cost-k" "real" 0.49)
    )))

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

(define p-oxide-conv 2.241)
(define k-oxide-conv 1.205)
(define s-oxide-conv 2.5)

(define (get-custom-type name)
  (map
   (lambda (e)
     ;; convert from elemental to oxide form here
     (list 
      ;; pass type through for converting to crop avail
      (ktv-get e "type")
      (ktv-get e "N") 
      (* (ktv-get e "P") p-oxide-conv) 
      (* (ktv-get e "K") k-oxide-conv)
      (* (ktv-get e "S") s-oxide-conv)
      (ktv-get e "M")))
   (db-filter-only 
    db "farm" "manure" 
    (list (list "name" "varchar" "=" name))
    (list (list "N" "real") 
	  (list "P" "real") 
	  (list "K" "real")
	  (list "S" "real")
	  (list "M" "real")
	  (list "type" "varchar")))))

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
     (let ((qualities (get-qualities-for-type-inc-custom v)))
       (calc-modify-quality ;; need to set a valid quality for this type
	(calc-modify-type c v)
	(if (null? qualities)
	    #f (car (get-qualities-for-type-inc-custom v)))))))) ;; argh - sets to first

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

(define (update-costs)
  (set! costs 
	(list 
	 ;; sqlite seems to return reals as strings, but when we 
	 ;; set them internally they are numbers WTF
	 (if (string? (entity-get-value "cost-n"))
	     (string->number (entity-get-value "cost-n"))
	     (entity-get-value "cost-n"))
	 (if (string? (entity-get-value "cost-p"))
	     (string->number (entity-get-value "cost-p"))
	     (entity-get-value "cost-p"))
	 (if (string? (entity-get-value "cost-k"))
	     (string->number (entity-get-value "cost-k"))
	     (entity-get-value "cost-k"))
	 ;; deal with auto upgrading
	 (let ((cost-s (entity-get-value "cost-s")))
	   (if (not cost-s) 0
	       (if (string? cost-s)
		   (string->number (entity-get-value "cost-s"))
		   (entity-get-value "cost-s"))))
	 (let ((cost-m (entity-get-value "cost-m")))
	   (if (not cost-m) 0
	       (if (string? cost-m)
		   (string->number (entity-get-value "cost-m"))
		   (entity-get-value "cost-m")))))))

(define rainfall 'rain-medium)

(define (update-rainfall)
  (set! rainfall (string->symbol (entity-get-value "rainfall"))))

(define (current-rainfall) rainfall)

(define (mutate-current-seek-mul! a)
  (set! calc (calc-modify-seek-mul calc a)))

;; apply modification to sulphur requirements based on nitrogen supply
;; for grazed grass on soils with sulphur deficiency (page 12 
;; grassland recommendations)
;;(define (deal-with-sulphur-nitrogen sulphur nitrogen)
;;  )

(define (run-calc)
  (let* ((nutrients (calc-nutrients))
	 (amounts (cadr nutrients))
	 (total-amounts (car nutrients))
	 (amount (calc-amount calc))
	 (type (calc-type calc))
	 (size (calc-fieldsize calc)))
    (append
     (list
      (update-widget 'text-view (get-id "amount-value") 'text
		     (string-append (convert-output->string amount (get-units)) " " (get-units)))
      
      (update-widget
       'seek-bar (get-id "amount")
       'redness (if (> (list-ref total-amounts 0) NVZ-nitrogen-warning)
		    255 0))
      
      ;; nutrient values: "crop-avail (total)"
      (update-widget 'text-view (get-id "na")
		     'text 
		     (convert-output->string (list-ref amounts 0) "kg/ha"))
      
      (update-widget 'text-view (get-id "pa")
		     'text 
		      (convert-output->string (list-ref amounts 1) "kg/ha"))
      
      (update-widget 'text-view (get-id "ka")
		     'text 
		      (convert-output->string (list-ref amounts 2) "kg/ha"))

      (update-widget 'text-view (get-id "sa")
		     'text 
		      (convert-output->string (list-ref amounts 3) "kg/ha"))

      (update-widget 'text-view (get-id "ma")
		     'text 
		      (convert-output->string (list-ref amounts 4) "kg/ha"))


      ;; nutrient values: "crop-total"
      (update-widget 'text-view (get-id "nt")
		     'text 
		      (convert-output->string (list-ref total-amounts 0) "kg/ha"))
      
      (update-widget 'text-view (get-id "pt")
		     'text 
		      (convert-output->string (list-ref total-amounts 1) "kg/ha"))
      
      (update-widget 'text-view (get-id "kt")
		     'text 
		      (convert-output->string (list-ref total-amounts 2) "kg/ha"))

      (update-widget 'text-view (get-id "st")
		     'text 
		      (convert-output->string (list-ref total-amounts 3) "kg/ha"))

      (update-widget 'text-view (get-id "mt")
		     'text 
		      (convert-output->string (list-ref total-amounts 4) "kg/ha"))
      
      
      
      
      ;; costs
      (update-widget 'text-view (get-id "costn")
		     'text (get-cost-string-from-nutrient 0 amounts size))
      (update-widget 'text-view (get-id "costp")
		     'text (get-cost-string-from-nutrient 1 amounts size))
      (update-widget 'text-view (get-id "costk")
		     'text (get-cost-string-from-nutrient 2 amounts size))
      (update-widget 'text-view (get-id "costs")
		     'text (get-cost-string-from-nutrient 3 amounts size))
      (update-widget 'text-view (get-id "costm")
		     'text (get-cost-string-from-nutrient 4 amounts size)))
     
     ;; still needed
     (if (eq? (get-current 'calc-mode #f) 'fieldcalc)
	 (begin
	   (list
	    (update-widget 'text-view (get-id "needed-n") 'text 
			   ;; need to check all these for n/a before the subtract...
			   (if (eq? (list-ref amounts 0) 'NA)
			       "N/A" (convert-output->string (- (entity-get-value "require-n") (list-ref amounts 0)) "kg/ha")))
	    (update-widget 'text-view (get-id "needed-p") 'text 
			   (if (eq? (list-ref amounts 1) 'NA)
			       "N/A" (convert-output->string (- (entity-get-value "require-p") (list-ref amounts 1)) "kg/ha")))
	    (update-widget 'text-view (get-id "needed-k") 'text 
			   (if (eq? (list-ref amounts 2) 'NA)
			       "N/A" (convert-output->string (- (entity-get-value "require-k") (list-ref amounts 2)) "kg/ha")))	    
	    (update-widget 'text-view (get-id "needed-s") 'text 
			   (if (eq? (list-ref amounts 3) 'NA)
			       "N/A" (convert-output->string (- (entity-get-value "require-s") (list-ref amounts 3)) "kg/ha")))	  ;; (deal-with-sulphur-nitgrogen (list-ref amounts 3) (list-ref amounts 0)))
	    (update-widget 'text-view (get-id "needed-m") 'text 
			   (if (eq? (list-ref amounts 4) 'NA)
			       "N/A" (convert-output->string (- (entity-get-value "require-m") (list-ref amounts 4)) "kg/ha")))	    
	    ;;;;;;
	    ;;(update-widget 'text-view (get-id "require-s") 'text
	    ;;		   (convert-output->string (list-ref results 3) "kg/ha"))
	    ;;;;
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
(define graph-s-col (map (lambda (x) (maximum 0 (- x 20))) '(255 149 0)))
(define graph-m-col (map (lambda (x) (maximum 0 (- x 20))) '(255 249 249)))

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
	      (y3 (- 250 (ktv-get event "nutrients-k")))
	      (y4 (if (ktv-get event "nutrients-s") (- 250 (ktv-get event "nutrients-s")) 250))
	      (y5 (if (ktv-get event "nutrients-m") (- 250 (ktv-get event "nutrients-m")) 250)))
	 (append
	  (if (< month-width 20)
	      (list
	       (drawlist-line graph-n-col 3 (list x 250 x y1))
	       (drawlist-line graph-p-col 3 (list (+ x 3) 250 (+ x 3) y2))
	       (drawlist-line graph-k-col 3 (list (+ x 6) 250 (+ x 6) y3))
	       (drawlist-line graph-s-col 3 (list (+ x 9) 250 (+ x 9) y4))
	       (drawlist-line graph-m-col 3 (list (+ x 12) 250 (+ x 12) y5)))
	      (list
	       (drawlist-line graph-n-col 10 (list x 250 x y1))
	       (drawlist-line graph-p-col 10 (list (+ x 5) 250 (+ x 5) y2))
	       (drawlist-line graph-k-col 10 (list (+ x 10) 250 (+ x 10) y3))
	       (drawlist-line graph-s-col 10 (list (+ x 15) 250 (+ x 15) y4))
	       (drawlist-line graph-m-col 10 (list (+ x 20) 250 (+ x 20) y5))))
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
     (drawlist-text "S" 280 120 graph-s-col 20 "horizontal")
     (drawlist-text "M" 280 150 graph-m-col 20 "horizontal")
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

;; just for graph/backup so don't have to be accurate!!!
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

(define (month->season m)
  (cond
   ((or (eq? m 'feb)
	(eq? m 'mar)
	(eq? m 'apr)) 'spring)
   ((or (eq? m 'may)
	(eq? m 'jun)
	(eq? m 'jul)) 'summer)
   ((or (eq? m 'aug)
	(eq? m 'sep)
	(eq? m 'oct)) 'autumn)
   (else 'winter)))

(define (date->month d)
  (cond
   ((eqv? (list-ref d 1) 1) 'jan)
   ((eqv? (list-ref d 1) 2) 'feb)
   ((eqv? (list-ref d 1) 3) 'mar)
   ((eqv? (list-ref d 1) 4) 'apr)
   ((eqv? (list-ref d 1) 5) 'may)
   ((eqv? (list-ref d 1) 6) 'jun)
   ((eqv? (list-ref d 1) 7) 'jul)
   ((eqv? (list-ref d 1) 8) 'aug)
   ((eqv? (list-ref d 1) 9) 'sep)
   ((eqv? (list-ref d 1) 10) 'oct)
   ((eqv? (list-ref d 1) 11) 'nov)
   (else 'dec)))

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
     (let ((crop-params (text->params-list (ktv-get field "crop"))))
       (let ((crop (if (null? crop-params)
		       'grass
		       (get-choice-value crop-params 'crop))))
	 (list
	  (ktv-get field "name")
	  (ktv-get field "unique_id")
	  ;; info text
	  (list 
	   ;; deal with bw compat with pre-parameterised crop data
	   (mtext-lookup crop)
	   (string-append 
	    (convert-output->string (ktv-get field "size") "hectares")
	    (if (eq? (current-units) 'imperial) "acres" "ha")))
	  (get-field-polygon (ktv-get field "unique_id"))))))
   (db-filter 
    db "farm" "field" 
    (list
     (list "parent" "varchar" "=" (get-setting-value "current-farm"))))))


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

(define (field-exists-yet? field-uid polygons)
  (> (length (get-field-polygon field-uid)) 0))

(define (get-field-centre field-uid polygons)
  (let ((r (polygon-centroid (get-field-polygon field-uid))))
    ;; default to farm centre if there is no field poly yet
    (if (and (eqv? (car r) 0) (eqv? (cadr r) 0))
	(get-farm-centre polygons)
	r)))

(define (polygons-empty? p)
  (null? p))
;; removed as it zooms out when the first field is empty
;; (null? (list-ref (car p) 3))))

(define (event-view-item id title bg)
  (linear-layout
   0
   'horizontal 
   (layout 'fill-parent 'wrap-content 1 'centre 5)
   (if bg list-colour (list 0 0 0 0))
   (list
    (text-view (make-id (string-append id "-text")) (mtext-lookup title) 20 (layout 'fill-parent 'wrap-content 0.5 'left 0))
    (text-view (make-id id) "" 30 (layout 'fill-parent 'wrap-content 0.5 'right 0)))))

(define (crop-params->readable-text crop-params)
  (cond
   ((null? crop-params) "")
   ((null? (cdr crop-params)) (mtext-lookup (cadr (car crop-params))))
   (else
    (string-append (mtext-lookup (cadr (car crop-params))) "/" 
		   (crop-params->readable-text (cdr crop-params))))))

(define (crop-params->spreadsheet-text crop-params)
  (cond
   ((null? crop-params) "")
   ((null? (cdr crop-params)) 
    (string-append
     (mtext-lookup (car (car crop-params))) ":"
     (mtext-lookup (cadr (car crop-params)))))
   (else
    (string-append 
     (mtext-lookup (car (car crop-params))) ": "
     (mtext-lookup (cadr (car crop-params))) " / " 
     (crop-params->readable-text (cdr crop-params))))))

(define (update-event-view-item id)
  (cond
   ((equal? id "crop")
    (update-widget 'text-view (get-id id) 'text (crop-params->readable-text (text->params-list (entity-get-value id)))))
   ((equal? id "size")
    (update-widget 'text-view (get-id id) 'text (convert-output->string (entity-get-value id) "hectares")))
   ((equal? id "amount")
    (update-widget 'text-view (get-id id) 'text (convert-output->string (entity-get-value id) (get-units-for-type (string->symbol (entity-get-value "type"))))))
   (else
    (update-widget 'text-view (get-id id) 'text (entity-get-value id)))))

(define (update-event-view-item-lookup id)
  (update-widget 'text-view (get-id id) 'text (mtext-lookup (string->symbol (entity-get-value id)))))

(define calc-text-size 30)
(define title-height 40)
(define units-width 130)

(define (calc-results)
  (vert
   (image-view (make-id "example") "test" (layout 'fill-parent 'fill-parent 1 'centre 0))
   (spacer 10)
   (linear-layout
    0 'horizontal
    (layout 'fill-parent 'wrap-content 1 'left 2) list-colour
    (list
     (image-view (make-id "im") "arrow_left" (layout 200 'fill-parent 1 'left 0))
     
     (scroll-view
      0 (layout 'wrap-content 'wrap-content 1 'left 0)
      (list   
       (horiz-colour
	(list 0 0 0 0)
	(vert
	 
	 (text-view 0 (mtext-lookup 'nutrients)
		    calc-text-size (layout 'wrap-content title-height 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "crop-total") (mtext-lookup 'crop-total-metric)
		    calc-text-size (layout 'wrap-content 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "crop-available") (mtext-lookup 'crop-available-metric)
		    calc-text-size (layout 'wrap-content 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view 0 (mtext-lookup 'cost-saving)
		    calc-text-size (layout 'wrap-content 'wrap-content 1 'left 0))
	 )
	(spacer-horiz 10)
	
	(vert
	 (text-view 0 (mtext-lookup 'nutrient-n)
		    calc-text-size (layout units-width title-height 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "nt") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "na") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "costn") "" calc-text-size (layout units-width 'wrap-content 1 'left 0)))
	(spacer-horiz 10)

	(vert
	 (text-view 0 (mtext-lookup 'nutrient-p)
		    calc-text-size (layout units-width title-height 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "pt") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "pa") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "costp") "" calc-text-size (layout units-width 'wrap-content 1 'left 0)))
	(spacer-horiz 10)

	(vert
	 (text-view 0 (mtext-lookup 'nutrient-k)
		    calc-text-size (layout units-width title-height 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "kt") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "ka") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "costk") "" calc-text-size (layout units-width 'wrap-content 1 'left 0)))
	(spacer-horiz 10)
	
	(vert
	 (text-view 0 (mtext-lookup 'nutrient-s)
		    calc-text-size (layout units-width title-height 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "st") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "sa") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "costs") "" calc-text-size (layout units-width 'wrap-content 1 'left 0)))
	(spacer-horiz 10)

	(vert
	 (text-view 0 (mtext-lookup 'nutrient-m)
		    calc-text-size (layout units-width title-height 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "mt") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "ma") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "costm") "" calc-text-size (layout units-width 'wrap-content 1 'left 0)))

	)))
     (image-view (make-id "im") "arrow_right" (layout 200 'fill-parent 1 'left 0))))
   
   (spacer 10)))


(define (calc-event-results)
  (vert
   (image-view (make-id "example") "test" (layout 'fill-parent 'fill-parent 1 'centre 0))
   (spacer 10)
   (linear-layout
    0 'horizontal
    (layout 'fill-parent 'wrap-content 1 'left 2) list-colour
    (list
     (image-view (make-id "im") "arrow_left" (layout 200 'fill-parent 1 'left 0))
     
     (scroll-view
      0 (layout 'wrap-content 'wrap-content 1 'left 0)
      (list   
       (horiz-colour
	(list 0 0 0 0)
	(vert
	 
	 (text-view 0 (mtext-lookup 'nutrients)
		    calc-text-size (layout 'wrap-content title-height 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "crop-total") (mtext-lookup 'crop-total-metric)
		    calc-text-size (layout 'wrap-content 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "crop-available") (mtext-lookup 'crop-available-metric)
		    calc-text-size (layout 'wrap-content 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view 0 (mtext-lookup 'crop-requirements)
		    calc-text-size (layout 'wrap-content 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view 0 (mtext-lookup 'still-needed)
		    calc-text-size (layout 'wrap-content 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view 0 (mtext-lookup 'cost-saving)
		    calc-text-size (layout 'wrap-content 'wrap-content 1 'left 0))
	 )
	(spacer-horiz 10)
	
	(vert
	 (text-view 0 (mtext-lookup 'nutrient-n)
		    calc-text-size (layout units-width title-height 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "nt") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "na") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "require-n") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "needed-n") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "costn") "" calc-text-size (layout units-width 'wrap-content 1 'left 0)))
	(spacer-horiz 10)

	(vert
	 (text-view 0 (mtext-lookup 'nutrient-p)
		    calc-text-size (layout units-width title-height 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "pt") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "pa") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "require-p") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "needed-p") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "costp") "" calc-text-size (layout units-width 'wrap-content 1 'left 0)))
	(spacer-horiz 10)

	(vert
	 (text-view 0 (mtext-lookup 'nutrient-k)
		    calc-text-size (layout units-width title-height 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "kt") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "ka") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "require-k") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "needed-k") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "costk") "" calc-text-size (layout units-width 'wrap-content 1 'left 0)))
	(spacer-horiz 10)
	
	(vert
	 (text-view 0 (mtext-lookup 'nutrient-s)
		    calc-text-size (layout units-width title-height 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "st") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "sa") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "require-s") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "needed-s") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "costs") "" calc-text-size (layout units-width 'wrap-content 1 'left 0)))
	(spacer-horiz 10)

	(vert
	 (text-view 0 (mtext-lookup 'nutrient-m)
		    calc-text-size (layout units-width title-height 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "mt") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "ma") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "require-m") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "needed-m") "" calc-text-size (layout units-width 'wrap-content 1 'left 0))
	 (spacer 10)
	 (text-view (make-id "costm") "" calc-text-size (layout units-width 'wrap-content 1 'left 0)))

	)))
     (image-view (make-id "im") "arrow_right" (layout 200 'fill-parent 1 'left 0))))
   
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
	 (if (eq? v 'fym)
	     (update-widget 'text-view (get-id "quality") 'text (mtext-lookup 'livestock-type))
	     (update-widget 'text-view (get-id "quality") 'text (mtext-lookup 'quality)))
	 
	 (update-widget 'seek-bar (get-id "amount") 'progress 50)
	 (update-widget 'image-view (get-id "example") 'image
			(find-image (calc-type calc)
				    (calc-quality calc)
				    (calc-amount calc))))
	;; enable/disable the qualities and applications depending on the manure type
	(let ((qualities (get-qualities-for-type-inc-custom v)))
	  (if (null? qualities)
	      (list
	       (update-widget 'spinner (get-id "quality-spinner") 'disabled 0)
	       (update-widget 'spinner (get-id "quality-spinner") 'array '("None")))
	      (list
	       (update-widget 'spinner (get-id "quality-spinner") 'enabled 0)
	       (update-widget 'spinner (get-id "quality-spinner") 'array
			      (symbol-list-to-names qualities)))))
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
			    (msg "setting custom manure to" (car custom))
			    (set-custom-override! (car custom)))		      
		      (fn name) '())))
		 (else
		  (set-custom-override! #f)
		  (let ((qualities (get-qualities-for-type type)))
		    ;; some manures now have no qualities!
		    (when (not (null? qualities))
			  (let ((quality 
				 (list-ref (get-qualities-for-type type) v)))
			    (fn quality)
			    (append
			     (list
			      (update-widget 'image-view (get-id "example") 'image					     
					     (find-image (calc-type calc)
							 quality
							 (calc-amount calc))))			      
			     (update-quality! quality)))))))))))
	    
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
					     (calc-quality calc)
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
   (get-crop-requirements/supply-from-current
    (date->month (current-date)))))

(define (update-crop-details-from-current)
  (let ((crop-params (text->params-list (entity-get-value "crop"))))
    (list
     ;; main crop name
     (update-widget 'text-view (get-id "crop-output") 'text 
		    (mtext-lookup (get-choice-value crop-params 'crop)))
     ;; name-value pairs as details
     (update-widget
      'linear-layout
      (get-id "crop-details")
      'contents
      (foldl
       (lambda (param r)
	 (if (not (eq? (car param) 'crop))
	     (append 
	      r (list 
		 (text-view 0 (string-append 
			       (mtext-lookup (car param))
			       ": " 
			       (mtext-lookup (cadr param))) 
			    20 fillwrap)))
	     r))
       ()
       crop-params)))))

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
		      (soil-nutrient-code-to-text (list-ref results 5)))
   (update-widget 'text-view (get-id "risk-s") 'text (mtext-lookup (list-ref results 6)))
   (update-widget 'text-view (get-id "require-n") 'text (convert-output->string (list-ref results 0) "kg/ha"))
   (update-widget 'text-view (get-id "require-p") 'text (convert-output->string (list-ref results 1) "kg/ha"))
   (update-widget 'text-view (get-id "require-k") 'text (convert-output->string (list-ref results 2) "kg/ha"))
   (update-widget 'text-view (get-id "require-s") 'text (convert-output->string (list-ref results 3) "kg/ha"))
   (update-widget 'text-view (get-id "require-m") 'text (convert-output->string (list-ref results 4) "kg/ha"))))

(define (get-crop-requirements/supply-from-field field month)
  (get-crop-requirements/supply 
   (current-rainfall)
   (text->params-list (ktv-get field "crop"))
   (string->symbol (ktv-get field "soil"))
   (string->symbol (ktv-get field "previous-crop"))
   (string->symbol (ktv-get field "regularly-manure"))
   (string->symbol (ktv-get field "soil-test-p"))
   (string->symbol (ktv-get field "soil-test-k"))
   (string->symbol (ktv-get field "soil-test-m"))
   (string->symbol (ktv-get field "recently-grown-grass"))
   month))

(define (get-crop-requirements/supply-from-current month)
  (get-crop-requirements/supply 
   (current-rainfall)
   (text->params-list (entity-get-value "crop"))
   (string->symbol (entity-get-value "soil"))
   (string->symbol (entity-get-value "previous-crop"))
   (string->symbol (entity-get-value "regularly-manure"))
   (string->symbol (entity-get-value "soil-test-p"))
   (string->symbol (entity-get-value "soil-test-k"))
   (string->symbol (entity-get-value "soil-test-m"))
   (string->symbol (entity-get-value "recently-grown-grass"))
   month))

;;---------------------------------------------------------------


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
   "Manure total N,"
   "Manure total P,"
   "Manure total K,"
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
	     '("total-nutrients-n" "real")
	     '("total-nutrients-p" "real")
	     '("total-nutrients-k" "real")
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
			     (list "soil" "application" "season"))		   
		   (string-append r ", \"" (mtext-lookup (string->symbol (ktv-value ktv))) "\""))

		  ((equal? (ktv-key ktv) "crop")
		   (string-append r ", \"" (crop-params->spreadsheet-text (text->params-list (ktv-value ktv))) "\""))
		  
		  ((equal? (ktv-key ktv) "quality")		   
		   (if (eq? manure-type 'custom-manure)
 		       (string-append r ", \"" (ktv-value ktv) "\"")
		       (string-append r ", \"" (mtext-lookup (string->symbol (ktv-value ktv))) "\"")))	 
	  
		  ;; convert value
		  ((in-list? (ktv-key ktv)
			     (list "nutrients-n" "nutrients-p" "nutrients-k" "nutrients-s" "nutrients-m" 
				   "require-n" "require-p" "require-k" "require-s" "require-m"))		   
		   (string-append r ", \"" (convert-output->string (ktv-value ktv) "kg/ha") "\""))
		  
		  ((equal? (ktv-key ktv) "sns")
		   (string-append 
		    r ", \"" 
		    (soil-nutrient-code-to-text (ktv-value ktv))
		    "\""))

		  ((equal? (ktv-key ktv) "size")
		   (string-append r ", \"" (convert-output->string (ktv-value ktv) "hectares") "\""))
		  
		  ((equal? (ktv-key ktv) "amount")
		   (string-append 
		    r ", \"" (convert-output->string 
			      (ktv-value ktv) 
			      (get-metric/imperial-units-for-type manure-type))
		    "\"")) 
		  
		  (else
		   (string-append r ", \"" (stringify-value-url ktv) "\""))))
	       ""
	       entity))))
	 (crap-titles)
	 s))))

(define (remove-dashes str)
  (foldl
   (lambda (c r)
     (if (equal? c #\-)
         (string-append r "_")
         (string-append r (string c))))
   ""
   (string->list str)))

(define (remove-underscores str)
  (foldl
   (lambda (c r)
     (if (equal? c #\_)
         (string-append r "-")
         (string-append r (string c))))
   ""
   (string->list str)))

(define (escape-single-quotes str)
  (foldl
   (lambda (c r)
     (if (equal? c #\')
         (string-append r "\\'")
         (string-append r (string c))))
   ""
   (string->list str)))

(define (entity->assoc entity)
  (map 
   (lambda (ktv)
     (if (equal? (ktv-key ktv) "crop")
	 ;; parse crops back into scheme so then can be
	 ;; converted correctly into json - to avoid nasty
	 ;; quote escaping nightmares
	 ;;(cons "crop" (json/parse-string (ktv-value ktv)))
	 ;; just try the string as aidan does it...
	 (cons "crop" (escape-single-quotes (ktv-value ktv)))
	 ;; make it an assoc list
	 (cons
	  (remove-dashes (ktv-key ktv))	 
	  (ktv-value ktv))))
   (ktv-filter-many entity (list "user" "time"))))

(define current-export-version 1)

(define (export-current-farm-as-json)
  (let ((farm-id (get-setting-value "current-farm")))
    (assoc->json
     (list
      (cons 
       "farm" 
       (append 
	(list (cons "file-version" current-export-version))
	(list (cons "app-version" app-version))
	(entity->assoc (get-entity-by-unique db "farm" farm-id))
	(list 
	 (cons 
	  "fields"
	  (map 
	   (lambda (field)
	     (append
	      (entity->assoc field)
	      (list 
	       (cons "coords"
		     (map
		      (lambda (coord) 
			(entity->assoc (ktv-filter-keep (list "name" "order" "lat" "lng" "parent") coord)))
		      (db-filter 
		       db "farm" "coord" 
		       (list
			(list "parent" "varchar" "=" (ktv-get field "unique_id"))))))
	       (cons "events"
		     (map
		      (lambda (event) 
			(entity->assoc event))
		      (db-filter 
		       db "farm" "event" 
		       (list
			(list "parent" "varchar" "=" (ktv-get field "unique_id")))))))
	      ))
	   (db-filter 
	    db "farm" "field" 
	    (list
	     (list "parent" "varchar" "=" farm-id))))))))))))

;;; importing

;; 1. look for existing farms with this unique_id
;;  a) if not found - simple, we can just add new farm in
;;  b) if found, merge farms
;; 
;; 2. merging farms
;;  a) overwrite all farm specific data with import
;;  b) for each field in import - check existing fields with unique_id
;;  c) add field if not found in farm, or merge existing field
;; 
;; 3. merging fields
;;  a) overwrite all field specific data with import (including gps coords)
;;  b) for each event in import - check existing events with unique_id
;;  c) add event if not found in field, or overwrite with import
;; 
;; (overwriting existing events with import is not strictly required, as
;; they are immutable, but it follows the pattern so...)

;; data is a key value list from the json object
;; keys are symbols for some reason (json parser)

;; need to pre-register attribute types as they may not be created 
;; before we import

(define export-attributes
  '(("farm" (("name" "varchar") 
	     ("deleted" "int")
	     ("cost-n" "real") 
	     ("cost-p" "real") 
	     ("cost-k" "real") 
	     ("cost-s" "real")
	     ("cost-m" "real")  
	     ("rainfall" "varchar")))
    ("field" (("name" "varchar")
	      ("deleted" "int")
	      ("parent" "varchar")
	      ("soil" "varchar")
	      ("crop" "varchar")
	      ("previous-crop" "varchar")
	      ("soil-test-p" "varchar")
	      ("soil-test-k" "varchar")
	      ("soil-test-m" "varchar")
	      ("regularly-manure" "varchar")
	      ("recently-grown-grass" "varchar")
	      ("size" "real")))
    ("coord" (("name" "varchar")
	      ("deleted" "int")
	      ("parent" "varchar")
	      ("order" "int")
	      ("lat" "real")
	      ("lng" "real")))
    ("event" (("name" "varchar")
	      ("deleted" "int")
	      ("parent" "varchar")
	      ("type" "varchar")
	      ("date" "varchar")
	      ("nutrients-n" "real")
	      ("nutrients-p" "real")
	      ("nutrients-k" "real")
	      ("nutrients-s" "real")
	      ("nutrients-m" "real")
	      ("total-nutrients-n" "real")
	      ("total-nutrients-p" "real")
	      ("total-nutrients-k" "real")
	      ("total-nutrients-s" "real")
	      ("total-nutrients-m" "real")
	      ("require-n" "real")
	      ("require-p" "real")
	      ("require-k" "real")
	      ("require-s" "real")
	      ("require-m" "real")
	      ("sns" "int")
	      ("soil" "varchar")
	      ("size" "real")
	      ("amount" "real")
	      ("quality" "varchar")
	      ("application" "varchar")
	      ("season" "varchar")
	      ("crop" "varchar")))))

(define (find-import-attribute-type entity-type key)
  (let ((kp (assoc key (cadr (assoc entity-type export-attributes)))))
    (if kp (cadr kp) '())))

(define (build-ktv-list-from-import db table entity-type data)
  (foldl
   (lambda (kv-pair r)
     (if (eq? (car kv-pair) 'unique_id)
	 r
	 (let* ((key (remove-underscores (symbol->string (car kv-pair))))
		(attribute-type (find-import-attribute-type entity-type key)))
	   (cond
	    ((null? attribute-type) (msg "unknown import key: for " key) r)
	    ;; crop is a string of a json object...
	    ((eq? (car kv-pair) 'crop)
	     ;;(cons (ktv key attribute-type (dbg (params-list->text (cdr kv-pair)))) r)
	     (cons (ktv key attribute-type (cdr kv-pair)) r))
	    (else
	     (cons (ktv key attribute-type (cdr kv-pair)) r))))))
   '()
   data))

;; returns true if exists, false if new
(define (import-entity db table entity-type data)
  (let ((uid-pair (assoc 'unique_id data)))
    (cond
     (uid-pair
      (let* ((uid (cdr uid-pair))
	     (entity-id (entity-id-from-unique db table uid)))
	;;(msg "searched for entity:" entity-id)
	(cond 
	 ((not (null? entity-id))
	  ;; it exists!
	  (update-entity-values 
	   db table entity-id 
	   (build-ktv-list-from-import 
	    db table entity-type data)
	   #f) ;; dirtify value - ignored anyway here...
	  #t)
	 (else
	  ;; it doesn't yet
	  (insert-entity-wholesale 
	   db table entity-type uid 0 0 
	   (build-ktv-list-from-import db table entity-type data))
	  #f))))
     (else
      (msg "no uid in imported entity" data)
      #f))))

;; some functions for inspecting the data pre-inport
(define (farm-exists? db table import-data)
  (let ((farm-data (cdr (car import-data))))
    (let ((uid-pair (assoc 'unique_id farm-data)))
      (if uid-pair
	  (let* ((uid (cdr uid-pair))
		 (entity-id (entity-id-from-unique db table uid)))
	    (not (null? entity-id)))
	  #f))))

;; for checking which ones to import - not used yet...
(define (existing-fields db table import-data)
  (let ((farm-data (cdr (car import-data))))
    (let ((fields-list (cdr (assoc 'fields farm-data))))
      (foldl
       (lambda (field-data r)
	 (if (not (null? (entity-id-from-unique db table (assoc 'unique_id field-data))))
	     (cons (list (assoc 'unique_id field-data)
			 (assoc 'name field-data)) r)
	     r))
       '()
       fields-list))))

(define (farm-name import-data)
  (let ((farm-data (cdr (car import-data))))
    (cdr (assoc 'name farm-data))))


(define (import-farm db table import-data)
  (let ((farm-data (cdr (car import-data))))
    (let ((version (cdr (assoc 'file-version farm-data))))
      (cond 
       ((eqv? version current-export-version)	  
	(import-entity db table "farm" farm-data)
	(let ((fields-list (cdr (assoc 'fields farm-data))))
	  (foldl
	   (lambda (field-data r)
	     ;; import fields
	     (let ((field-exists (import-entity db table "field" field-data)))
	       ;; import polygons....
	       ;; delete previous polygon if one exists
	       (db-delete-children db "farm" "coord" (cdr (assoc 'unique_id field-data)))
	       (let ((coords-list (cdr (assoc 'coords field-data))))
		 (for-each
		  (lambda (coord-data)
		    ;; plain old insert-entity as we have cleared this first
		    ;; and have no uids
		    (insert-entity
		     db "farm" "coord" "sys" 
		     (build-ktv-list-from-import db "farm" "coord" coord-data)))
		  coords-list))	
	       ;; import events.......
	       (let ((events-list (cdr (assoc 'events field-data))))
		 ;; return a list of imported fields and events
		 (cons 
		  (list 'field (cdr (assoc 'name field-data)) field-exists)
		  (foldl
		   (lambda (event-data r)
		     (cons
		      (list 'event (cdr (assoc 'date event-data))
			    (import-entity db table "event" event-data))
		      r))
		   r
		   events-list)))))
	   '()
	   fields-list)))
       (else #f)))))

(define (build-import-report import-result farm-name)
  (update-widget
   'linear-layout
   (get-id "import-report")
   'contents
   (cons
    (text-view 
     0 
     (string-append (mtext-lookup 'import-report) farm-name)
     large-text-size (layout 'wrap-content 'wrap-content -1 'left 0))
    (map
     (lambda (field-details)
       (text-view 
	0 (string-append 
	   (mtext-lookup 
	    (if (eq? (car field-details) 'field)
		(if (list-ref field-details 2) 'overwritten-field 'new-field)
		(if (list-ref field-details 2) 'overwritten-event 'new-event)))
	   ;; name 
	   (cadr field-details))
	normal-text-size (layout 'wrap-content 'wrap-content -1 'left 0)))
     import-result))))

(define (backup-days)
  (let ((today (date->day (date->string (current-date))))
	(last (get-setting-value "last-backup")))
    (if (or (not last) (equal? last ""))
	"never."
	(string-append (number->string (- today (date->day last))) 
		       " days ago."))))
	
(define (check-backup)
  (let ((backup-freq (get-setting-value "backup-freq")))
    (if backup-freq
	(let ((today (date->day (date->string (current-date))))
	      (freq (cond
		     ((equal? backup-freq "daily") 1)
		     ((equal? backup-freq "weekly") 7)
		     (else 30)))
	      (last (get-setting-value "last-backup")))
	  (cond
	   ((and (not (equal? backup-freq "never"))	       
		 (or (not last)
		     (equal? last "")
		     (>= (- today (date->day last))
			 freq)))
	    (list
	     (alert-dialog
	      "timed-backup"
	      (string-append (mtext-lookup 'timed-backup-are-you-sure)
			     (backup-days))
	      (lambda (v)
		(if (eqv? v 1)
		    (list (start-activity "backup" 2 ""))
		    '())))))
	   (else '())))
	'())))
