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

(msg "crap-app.scm")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(msg "crap-app.scm2")

(define db "/sdcard/farmcrapapppro/crapapp.db")
(set-current! 'db db)

(define (setup-database!)
  (msg "setting up database")
  (db-close db) ;; close just in case (sorts out db file delete while running problem)
  (db-open db)
  (msg "setting up tables")
  (setup db "local")
  (setup db "farm")
  (msg (db-status db))
  (insert-entity-if-not-exists
   db "local" "app-settings" "null" settings-entity-id-version
   (list
    (ktv "user-id" "varchar" "None Yet")
    (ktv "language" "int" 0)
    (ktv "email" "varchar" "None Yet")
    (ktv "units" "varchar" "metric"))))


(define (event-type e) (list-ref e 1))
(define (event-date e) (list-ref e 2))
(define (event-nutrients e) (list-ref e 3))
(define (event-amount e) (list-ref e 4))
(define (event-quality e) (list-ref e 5))
(define (event-season e) (list-ref e 6))
(define (event-crop e) (list-ref e 7))
(define (event-soil e) (list-ref e 8))
(define (event-size e) (list-ref e 9))
(define (event-units e) (list-ref e 10))

(define (field-name) (ktv-get (get-current 'field-values '()) "name"))
(define (field-soil) (ktv-get (get-current 'field-values '()) "soil"))
(define (field-crop) (ktv-get (get-current 'field-values '()) "crop"))
;;(define (field-events f) (list-ref f 3))
(define (field-size) (ktv-get (get-current 'field-values '()) "size"))


(define (calculator-state)
  (list
   (calc 'pig 25 'DM2 'autumn 'normal 'mediumheavy)
   (list date-day date-month date-year)
   1))

(define (state-calc s) (list-ref s 0))
(define (state-modify-calc s v) (list-replace s 0 v))
(define (state-date s) (list-ref s 1))
(define (state-modify-date s v) (list-replace s 1 v))
(define (state-seek-mul s) (list-ref s 2))
(define (state-modify-seek-mul s v) (list-replace s 2 v))

(define (calc type amount quality season crop soil)
  (list type amount quality season crop soil))

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

(define (update-calc! fn)
  (mutate-state!
   (lambda (s)
     (state-modify-calc s (fn (state-calc s)))))
  (run-calc))

(define (get-qualities-for-type t)
  (cond
   ((eq? t 'cattle) cattle-quality-list)
   ((eq? t 'pig) pig-quality-list)
   ((eq? t 'poultry) poultry-quality-list)
   (else fym-quality-list)))

(define (update-type! v) 
  (update-calc! 
   (lambda (c) 
     (calc-modify-quality ;; need to set a valid quality for this type
      (calc-modify-type c v)
      (car (get-qualities-for-type v))))))

(define (update-amount! v) (update-calc! (lambda (c) (calc-modify-amount c v))))
(define (update-quality! v) (update-calc! (lambda (c) (calc-modify-quality c v))))
(define (update-season! v) (update-calc! (lambda (c) (calc-modify-season c v))))
(define (update-crop! v) (update-calc! (lambda (c) (calc-modify-crop c v))))
(define (update-soil! v) (update-calc! (lambda (c) (calc-modify-soil c v))))

(define gcalculator-state (calculator-state))

(define (mutate-state! fn)
  (set! gcalculator-state (fn gcalculator-state)))

(define (mutate-units! v)
  (set-setting! "units" "varchar" (symbol->string v)))

(define (current-units)
  (string->symbol (get-setting-value "units")))
 
(define (mutate-email! v)
  (set-setting! "email" "varchar" v))

(define (current-email)
  (get-setting-value "email"))

(define (current-date) (state-date gcalculator-state))
(define (current-calc) (state-calc gcalculator-state))
(define (current-seek-mul) (state-seek-mul gcalculator-state))

(define (mutate-current-seek-mul! a)
  (msg "updating seek" a)
  (mutate-state!
   (lambda (s)
     (state-modify-seek-mul s a))))


;; (define (csv-headings)
;;   (string-append
;;    "Sield name\t"
;;    "Size\t"
;;    "Size units\t"
;;    "Soil\t"
;;    "Crop\t"
;;    "Manure\t"
;;    "Date\t"
;;    "N\t P\t K\t Nutrient units\t"
;;    "Amount\t"
;;    "Amount units\t"
;;    "Total Amount\t"
;;    "Total units\t"
;;    "Quality\t"
;;    "Season\t"
;;    "N Field Saving\t"
;;    "P Field Saving\t"
;;    "K Field Saving\t"
;;    "N Unit price\t"
;;    "P Unit price\t"
;;    "K Unit price\n"
;;    ))

;; (define (stringify-event event name soil crop size)
;;   (let ((aunits (amount-units event))
;;         (nunits (nutrient-units event)))
;;     (string-append
;;      name "\t"
;;      (number->string (convert-output size "hectares")) "\t"
;;      (if (equal? (current-units) metric) "ha" "acres") "\t"
;;      soil "\t"
;;      crop "\t"
;;      (event-type event) "\t"
;;      (date->string (event-date event)) "\t"
;;      (number->string (convert-output (list-ref (event-nutrients event) 0) "kg/ha")) "\t"
;;      (number->string (convert-output (list-ref (event-nutrients event) 1) "kg/ha")) "\t"
;;      (number->string (convert-output (list-ref (event-nutrients event) 2) "kg/ha")) "\t"
;;      (if (equal? (current-units) metric) "kg/ha" "units/acre") "\t"
;;      (number->string (convert-output (event-amount event) nunits)) "\t"
;;      nunits "\t"
;;      (number->string (convert-output (* size (event-amount event)) aunits)) "\t"
;;      aunits "\t"
;;      (event-quality event) "\t"
;;      (event-season event) "\t"
;;      "£" (get-cost-string-from-nutrient 0 (event-nutrients event) size) "\t"
;;      "£" (get-cost-string-from-nutrient 1 (event-nutrients event) size) "\t"
;;      "£" (get-cost-string-from-nutrient 2 (event-nutrients event) size) "\t"
;;      "£" (padcash->string (list-ref costs 0)) "\t"
;;      "£" (padcash->string (list-ref costs 1)) "\t"
;;      "£" (padcash->string (list-ref costs 2))
;;      )))

;; (define (stringify-field field)
;;   (foldl
;;    (lambda (event str)
;;      (string-append
;;       str
;;       (stringify-event
;;        event
;;        (field-name field)
;;        (field-soil field)
;;        (field-crop field)
;;        (field-size field)) "\n"))
;;    ""
;;    (field-events field)))

;; (define (stringify-fields)
;;   (foldl
;;    (lambda (field str)
;;      (string-append str (stringify-field field) "\n"))
;;    (csv-headings)
;;    (get-fields)))

(define (calc-nutrients)
  (let* ((type (calc-type (state-calc gcalculator-state)))
         (amount (calc-amount (state-calc gcalculator-state)))
         (quality (calc-quality (state-calc gcalculator-state)))
         (season (calc-season (state-calc gcalculator-state)))
         (crop (calc-crop (state-calc gcalculator-state)))
         (soil (calc-soil (state-calc gcalculator-state))))
;    (display type)(newline)
;    (display amount)(newline)
;    (display season)(newline)
;    (display quality)(newline)
;    (display crop)(newline)
;    (display soil)(newline)

    (get-nutrients type amount quality season crop soil)))

(define (current-type)
  (calc-type (state-calc gcalculator-state)))

(define (get-units)
  (let ((type (calc-type (state-calc gcalculator-state))))
    (msg "get units" (current-units))
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

(define (run-calc)
  (let ((amounts (calc-nutrients))
        (amount (calc-amount (state-calc gcalculator-state)))
        (type (calc-type (state-calc gcalculator-state))))
    (list
     (update-widget 'text-view (get-id "amount-value") 'text
                    (string-append (number->string (convert-output amount (get-units))) " " (get-units)))
     (update-widget 'text-view (get-id "na")
                    'text (number->string (convert-output (list-ref amounts 0) "kg/ha")))
     (update-widget 'text-view (get-id "pa")
                    'text (number->string (convert-output (list-ref amounts 1) "kg/ha")))
     (update-widget 'text-view (get-id "ka")
                    'text (number->string (convert-output (list-ref amounts 2) "kg/ha")))
     ;; costs
     (update-widget 'text-view (get-id "costn")
                    'text (get-cost-string-from-nutrient 0 amounts 1))
     (update-widget 'text-view (get-id "costp")
                    'text (get-cost-string-from-nutrient 1 amounts 1))
     (update-widget 'text-view (get-id "costk")
                    'text (get-cost-string-from-nutrient 2 amounts 1))
     )))

(define (spacer size)
  (space (layout 'fill-parent size 1 'left 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(msg "crap-app.scm3")

(define graph-width 320)

(define (build-lines events min max colour n)
  (let ((twidth (- max min)))
    (cadr (foldl
           (lambda (event r)
             (let* ((t (date->day (event-date event)))
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
  (let* ((twidth (- max min))
         (month-width (* (/ 30 twidth) graph-width)))
    (foldl
     (lambda (event r)
       (let* ((t (date->day (event-date event)))
              (x (* graph-width (/ (- t min) twidth)))
              (y1 (- 250 (list-ref (event-nutrients event) 0)))
              (y2 (- 250 (list-ref (event-nutrients event) 1)))
              (y3 (- 250 (list-ref (event-nutrients event) 2))))
         (append
          (if (< month-width 20)
              (list
               (drawlist-line '(200 0 0) 3 (list x 250 x y1))
               (drawlist-line '(200 200 0) 3 (list (+ x 3) 250 (+ x 3) y2))
               (drawlist-line '(0 0 200) 3 (list (+ x 6) 250 (+ x 6) y3)))
              (list
               (drawlist-line '(200 0 0) 10 (list x 250 x y1))
               (drawlist-line '(200 200 0) 10 (list (+ x 5) 250 (+ x 5) y2))
               (drawlist-line '(0 0 200) 10 (list (+ x 10) 250 (+ x 10) y3))))
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

  (let* ((twidth (- max min))
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
     (drawlist-text "N" 280 30 '(200 0 0) 20 "horizontal")
     (drawlist-text "P" 280 60 '(200 200 0) 20 "horizontal")
     (drawlist-text "K" 280 90 '(0 0 200) 20 "horizontal")
     )))

(define (build-graph)
  (append
   (let ((events (field-events (current-field))))
     (if (> (length events) 1)
         (let* ((_min (date->day (event-date (car events))))
                (_max (date->day (event-date (list-ref events (- (length events) 1)))))
                (safe (* (- _max _min) 0.1))
                (min (- _min safe))
                (max (+ _max safe)))
           (append
            (build-t-scale (event-date (car events)) min max)
            (build-bars events min max)
            (build-key)))
         (list (drawlist-text "Not enough events for graph"
                              20 105 '(0 0 0) 20 "horizontal"))))
   (list
    (drawlist-line '(0 0 0) 5 (list 0 0 320 0))
    (drawlist-line '(0 0 0) 5 (list 0 250 320 250)))))

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

(define (build-field-buttons)
  (if (null? (get-fields))
      (list (text-view (make-id "temp") "Add some fields" 20 fillwrap))
      (map
       (lambda (field)
         (button
          (make-id (field-name field))
          (field-name field)
          20 fillwrap
          (lambda ()
	    (set-current! 'field (field-name field))
            (list (start-activity "field" 2 "")))))
       (get-fields))))

(define (build-event-buttons)
  (if (null? (field-events (current-field)))
      (list (text-view (make-id "temp") "No events yet" 15 fillwrap))
      (map
       (lambda (event)
         (button
          (make-id (string-append
                    "event-"
                    ;; need to add field to prevent clashing with other field id numbers
                    (field-name (current-field))
                    (number->string (event-id event))))
          (string-append (event-type event)
                         " "
                         (date->string (event-date event)))
          15 fillwrap
          (lambda ()
            (mutate-current-event! (lambda (ev) event))
            (list
             (start-activity "eventview" 2 "")))))
       (field-events (current-field)))))

;; just for graph so don't have to be accurate!!!
(define (date->day d)
  (+ (* (list-ref d 2) 360)
     (* (list-ref d 1) 30)
     (list-ref d 0)))

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


(define (date->string d)
  (string-append
   (number->string (list-ref d 0))
   "/"
   (number->string (list-ref d 1))
   "/"
   (number->string (list-ref d 2))))

(define (date->season d)
  (msg "date->season" d)
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
      (map
       (lambda (coord)
	 (list
	  (ktv-get coord "lat") 
	  (ktv-get coord "lng")))
       (db-filter 
	db "farm" "coord" 
	(list
	 (list "parent" "varchar" "=" (ktv-get field "unique_id")))))))   
   (db-all db "farm" "field")))

(msg "crap-app.scm end")
