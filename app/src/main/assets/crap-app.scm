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

(define cattle "Cattle Slurry")
(define FYM "Farmyard Manure")
(define pig "Pig Slurry")
(define poultry "Poultry Litter")
(define normal "All crops")
(define grass-oilseed "Grassland/Winter oilseed rape")
(define sandyshallow "Sandy/Shallow")
(define mediumheavy "Medium/Heavy")
(define autumn "Autumn")
(define winter "Winter")
(define spring "Spring")
(define summer "Summer")
(define fresh "Fresh and soil incorporated")
(define other "Old and soil incorporated")
(define other1 "Fresh and surface applied")
(define other2 "Old and surface applied")

(define layer "Layer manure")
(define broiler "Broiler litter")
(define metric "Kg/ha")
(define imperial "units/acre")
(define DM2 "2% DM (Thin soup)")
(define DM4 "4% DM (Medium soup)")
(define DM6 "6% DM (Thick soup)")
(define DM10 "10% DM (Porridge)")
(define DM4-pig "4% DM (Thick soup)")
(define DM6-pig "6% DM (Porridge)")

(define costs (list 0.79 0.62 0.49))

(define images
  (list
   (list cattle
         (list
          (list 25 "cattle_25m3")
          (list 30 "cattle_30m3")
          (list 50 "cattle_50m3")
          (list 100 "cattle_100m3")))
   (list FYM
         (list
          (list 25 "fym_25t")
          (list 50 "fym_50t")))
   (list pig
         (list
          (list 25 "pig_25m3")
          (list 50 "pig_50m3")
          (list 75 "pig_75m3")))
   (list poultry
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
  (let ((type-images (cadr (_type images))))
    (cadr (_amount type-images (car type-images)))))

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
    ((equal? s autumn) (list-ref n 0))
    ((equal? s winter) (list-ref n 1))
    ((equal? s spring) (list-ref n 2))
    ((equal? s summer) (list-ref n 3))
    (else (error "season " s " not found") #f)))

(define (soil sandyshallow mediumheavy)
  (list sandyshallow mediumheavy))
(define (soil? s) (list? s))
(define (get-soil s t)
  (cond
    ((equal? t sandyshallow) (list-ref s 0))
    ((equal? t mediumheavy) (list-ref s 1))
    (else (error "soil type " t " not found") #f)))

(define (crop normal g)
  (list normal g))
(define (crop? c) (list? c))
(define (get-crop c t)
  (cond
    ((equal? t normal) (list-ref c 0))
    ((equal? t grass-oilseed) (list-ref c 1))
    (else (error "crop type " t " not found") #f)))


(define (find n l)
  (cond
    ((null? l) #f)
    ((equal? n (car (car l))) (car l))
    (else (find n (cdr l)))))

(define nutrients-metric
  (list
   (nutrients
    cattle "m3/ha" 100
    (list
     (quality DM2 (nitrogen (soil (crop 8 16) (crop 48 56))  48 72 56) 30 220)
     (quality DM6 (nitrogen (soil (crop 13 26) (crop 65 78)) 65 91 65) 60 290)
     (quality DM10 (nitrogen (soil (crop 18 36) (crop 72 90)) 72 90 72) 90 360)))
   (nutrients
    pig "m3/ha" 50
    (list
     (quality DM2 (nitrogen (soil (crop 15 22.5) (crop 52.5 60)) 60 82.5 82.5) 25 90)
     (quality DM4-pig (nitrogen (soil (crop 18 27) (crop 54 63)) 63 90 90) 45 110)
     (quality DM6-pig (nitrogen (soil (crop 22 33) (crop 55 66)) 66 99 99) 65 125)))
   (nutrients
    poultry "tons/ha" 10
    (list
     (quality layer (nitrogen (soil (crop 19 28.5) (crop 47.5 57)) 47.5 66.5 66.5) 84 86)
     (quality broiler (nitrogen (soil (crop 30 45) (crop 75 90)) (soil 60 75) 90 90) 150 162)))
   (nutrients
    FYM "tons/ha" 50
    (list
     (quality other (nitrogen (soil 15 30) 30 30 30) 95 360) ;; other
     (quality fresh (nitrogen (soil 15 30) 30 45 30) 95 360) ;; soil inc fresh
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

(define (get-nutrients type amount quality season crop soil)
  (let ((nutrients (find type nutrients-metric)))
    (if (not nutrients)
        (begin 
	  (error "nutrients type not found")
	  (display nutrients)(newline))
        (let ((q (find quality (nutrients-table nutrients))))
          (if (not q)
              (error "quality " quality " not found")
              (get-nutrients-inner
               (nutrients-amount nutrients)
               (nutrients-units nutrients)
               q amount season crop soil))))))

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

(define (imperial->metric amount units)
  (if (equal? (current-units) metric)
      amount
      (if (equal? units "m3/ha")
          (gallons/acre->m3/ha amount)
          (tons/acre->tons/ha amount))))

(define (metric->imperial amount units)
  (if (equal? (current-units) metric)
      amount
      (kg/ha->units/acre amount)))

(define (rounding a)
  (/ (round (* 10 a)) 10))

(define (rounding-cash a)
  (/ (round (* 100 a)) 100))

(define (padcash->string a)
  (let ((t (number->string (+ (rounding-cash a) 0.001))))
    (substring t 0 (- (string-length t) 1))))

(define (convert-input amount units)
  (if (equal? (current-units) metric)
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
   (if (equal? (current-units) metric)
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

;; quantity is from the table (so I can debug easily it matches the data)
;; amount is from the slider
(define (process-nutrients amount units quantity nutrients)
  (map
   (lambda (q)
     (rounding (* amount (/ q quantity))))
   nutrients))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (event id type date nutrients amount quality season crop soil size units)
  (list id type date nutrients amount quality season crop soil size units))

(define (empty-event)
  (event 0 "" (list 0 0 0) (list 0 0 0) 0 "" "" "" "" 0 metric))
(define (event-id e) (list-ref e 0))
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

(define (field name soil crop size)
  (list name soil crop '() size))

(define (empty-field)
  (field "" "" "" '() 50))

(define (field-name f) (list-ref f 0))
(define (field-modify-name f v) (list-replace f 0 v))
(define (field-soil f) (list-ref f 1))
(define (field-modify-soil f v) (list-replace f 1 v))
(define (field-crop f) (list-ref f 2))
(define (field-modify-crop f v) (list-replace f 2 v))
(define (field-events f) (list-ref f 3))
(define (field-modify-events f v) (list-replace f 3 v))
(define (field-size f) (list-ref f 4))
(define (field-modify-size f v) (list-replace f 4 v))

(define (field-add-event f event)
  (field-modify-events
   f
   (sort (append (field-events f) (list event))
         (lambda (a b)
           (date< (event-date a) (event-date b))))))

(define (field-remove-event f id)
  (field-modify-events
   f (filter
      (lambda (field)
        (not (equal? id (event-id field))))
      (field-events f))))

(define (state)
  (list
   (calc pig 25 DM2 autumn normal mediumheavy)
   (empty-field)
   (saved-data-load)
   (list date-day date-month date-year)
   (empty-event)
   1))

(define (state-calc s) (list-ref s 0))
(define (state-modify-calc s v) (list-replace s 0 v))
(define (state-field s) (list-ref s 1))
(define (state-modify-field s v) (list-replace s 1 v))
(define (state-saved-data s) (list-ref s 2))
(define (state-modify-saved-data s fn)
  (list-replace s 2 (saved-data-save (fn (state-saved-data s)))))
(define (state-date s) (list-ref s 3))
(define (state-modify-date s v) (list-replace s 3 v))
(define (state-event s) (list-ref s 4))
(define (state-modify-event s v) (list-replace s 4 v))
(define (state-seek-mul s) (list-ref s 5))
(define (state-modify-seek-mul s v) (list-replace s 5 v))

(define (saved-data fields)
  (list 0 fields 0 metric "your email"))
(define (saved-data-version f) (list-ref f 0))
(define (saved-data-fields f) (list-ref f 1))
(define (saved-data-modify-fields f v) (list-replace f 1 v))
(define (saved-data-next-event-id f) (list-ref f 2))
(define (saved-data-modify-next-event-id f v) (list-replace f 2 v))
(define (saved-data-units f) (list-ref f 3))
(define (saved-data-modify-units f v) (list-replace f 3 v))
(define (saved-data-email f) (list-ref f 4))
(define (saved-data-modify-email f v) (list-replace f 4 v))

(define (saved-data-modify-field fn name f)
  (saved-data-modify-fields
   f (map
      (lambda (field)
        (if (equal? (field-name field) name)
            (fn field)
            field))
      (saved-data-fields f))))

(define (fields-remove-field f name)
  (filter
   (lambda (field)
     (not (equal? name (field-name field))))
   f))

(define (saved-data-save d)
  (let ((f (open-output-file (string-append dirname "swarmhub-data.scm"))))
    (write d f)
    (close-output-port f))
  d)

(define (saved-data-load)
  (let* ((f (open-input-file (string-append dirname "swarmhub-data.scm"))))
    (if (not f)
        (saved-data-save (saved-data '()))
        (let ((r (read f)))
          (close-input-port f)
          ;; check versioning
          (if (not (number? (list-ref r 0)))
              ;; version 0
              (append (list 0) r (list "your email"))
              r)))))

(define (save-data filename d)
  (let ((f (open-output-file (string-append dirname filename))))
    (display d f)
    (close-output-port f))
  d)


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

(define (update-calc! pre fn)
  (mutate-state!
   (lambda (s)
     (state-modify-calc s (fn (state-calc s)))))
  (run-calc pre))

(define (update-type! pre v)
  (update-calc! pre (lambda (c) (calc-modify-type c v))))
(define (update-amount! pre v) (update-calc! pre (lambda (c) (calc-modify-amount c v))))
(define (update-quality! pre v) (update-calc! pre (lambda (c) (calc-modify-quality c v))))
(define (update-season! pre v) (update-calc! pre (lambda (c) (calc-modify-season c v))))
(define (update-crop! pre v) (update-calc! pre (lambda (c) (calc-modify-crop c v))))
(define (update-soil! pre v) (update-calc! pre (lambda (c) (calc-modify-soil c v))))

(define gstate (state))

(define (mutate-state! fn)
  (set! gstate (fn gstate)))

(define (mutate-saved-data! fn)
  (mutate-state!
   (lambda (s)
     (state-modify-saved-data
      s fn))))

(define (mutate-make-event-id!)
  (mutate-saved-data!
   (lambda (d)
     (saved-data-modify-next-event-id
      d (+ (saved-data-next-event-id d) 1))))
  (saved-data-next-event-id (state-saved-data gstate)))

(define (mutate-units! v)
  (mutate-saved-data!
   (lambda (d)
     (saved-data-modify-units d v))))

(define (current-units)
  (saved-data-units
   (state-saved-data gstate)))

(define (mutate-email! v)
  (mutate-saved-data!
   (lambda (d)
     (saved-data-modify-email d v))))

(define (current-email)
  (saved-data-email
   (state-saved-data gstate)))

(define (get-fields)
  (saved-data-fields (state-saved-data gstate)))

(define (current-field) (state-field gstate))
(define (current-date) (state-date gstate))
(define (current-calc) (state-calc gstate))
(define (current-event) (state-event gstate))
(define (current-seek-mul) (state-seek-mul gstate))

(define (mutate-current-seek-mul! a)
  (mutate-state!
   (lambda (s)
     (state-modify-seek-mul s a))))

(define (mutate-current-field! fn)
  (mutate-state!
   (lambda (s)
     (state-modify-field s (fn (state-field s))))))

(define (mutate-current-event! fn)
  (mutate-state!
   (lambda (s)
     (state-modify-event s (fn (state-event s))))))

(define (find-field name)
  (define (_ f)
    (cond
     ((null? f) #f)
     ((string=? (field-name (car f)) name)
      (car f))
     (else (_ (cdr f)))))
  (_ (get-fields)))

(define (csv-headings)
  (string-append
   "Sield name\t"
   "Size\t"
   "Size units\t"
   "Soil\t"
   "Crop\t"
   "Manure\t"
   "Date\t"
   "N\t P\t K\t Nutrient units\t"
   "Amount\t"
   "Amount units\t"
   "Total Amount\t"
   "Total units\t"
   "Quality\t"
   "Season\t"
   "N Field Saving\t"
   "P Field Saving\t"
   "K Field Saving\t"
   "N Unit price\t"
   "P Unit price\t"
   "K Unit price\n"
   ))

(define (stringify-event event name soil crop size)
  (let ((aunits (amount-units event))
        (nunits (nutrient-units event)))
    (string-append
     name "\t"
     (number->string (convert-output size "hectares")) "\t"
     (if (equal? (current-units) metric) "ha" "acres") "\t"
     soil "\t"
     crop "\t"
     (event-type event) "\t"
     (date->string (event-date event)) "\t"
     (number->string (convert-output (list-ref (event-nutrients event) 0) "kg/ha")) "\t"
     (number->string (convert-output (list-ref (event-nutrients event) 1) "kg/ha")) "\t"
     (number->string (convert-output (list-ref (event-nutrients event) 2) "kg/ha")) "\t"
     (if (equal? (current-units) metric) "kg/ha" "units/acre") "\t"
     (number->string (convert-output (event-amount event) nunits)) "\t"
     nunits "\t"
     (number->string (convert-output (* size (event-amount event)) aunits)) "\t"
     aunits "\t"
     (event-quality event) "\t"
     (event-season event) "\t"
     "£" (get-cost-string-from-nutrient 0 (event-nutrients event) size) "\t"
     "£" (get-cost-string-from-nutrient 1 (event-nutrients event) size) "\t"
     "£" (get-cost-string-from-nutrient 2 (event-nutrients event) size) "\t"
     "£" (padcash->string (list-ref costs 0)) "\t"
     "£" (padcash->string (list-ref costs 1)) "\t"
     "£" (padcash->string (list-ref costs 2))
     )))

(define (stringify-field field)
  (foldl
   (lambda (event str)
     (string-append
      str
      (stringify-event
       event
       (field-name field)
       (field-soil field)
       (field-crop field)
       (field-size field)) "\n"))
   ""
   (field-events field)))

(define (stringify-fields)
  (foldl
   (lambda (field str)
     (string-append str (stringify-field field) "\n"))
   (csv-headings)
   (get-fields)))

(define (calc-nutrients)
  (let* ((type (calc-type (state-calc gstate)))
         (amount (calc-amount (state-calc gstate)))
         (quality (calc-quality (state-calc gstate)))
         (season (calc-season (state-calc gstate)))
         (crop (calc-crop (state-calc gstate)))
         (soil (calc-soil (state-calc gstate))))
;    (display type)(newline)
;    (display amount)(newline)
;    (display season)(newline)
;    (display quality)(newline)
;    (display crop)(newline)
;    (display soil)(newline)

    (get-nutrients type amount quality season crop soil)))

(define (get-units)
  (let ((type (calc-type (state-calc gstate))))
    (if (equal? (current-units) metric)
        (nutrients-units (find type nutrients-metric))
        (if (equal? (nutrients-units (find type nutrients-metric)) "m3/ha")
            "gallons/acre"
            "tons/acre"))))

(define (nutrient-units event)
  (let ((type (event-type event)))
    (if (equal? (current-units) metric)
        (nutrients-units (find type nutrients-metric))
        (if (equal? (nutrients-units (find type nutrients-metric)) "m3/ha")
            "gallons/acre"
            "tons/acre"))))

(define (amount-units event)
  (let ((type (event-type event)))
    (if (equal? (current-units) metric)
        (if (equal? (nutrients-units (find type nutrients-metric)) "m3/ha")
            "m3" "tonnes")
        ;; it's imperial
        (if (equal? (nutrients-units (find type nutrients-metric)) "m3/ha")
            "gallons"
            "tonnes"))))

(define (get-cost-string-from-nutrient nutrient-index amounts mul)
  (padcash->string (* (list-ref amounts nutrient-index)
                      (list-ref costs nutrient-index) mul)))

(define (run-calc prepend)
  (let ((amounts (calc-nutrients))
        (amount (calc-amount (state-calc gstate)))
        (type (calc-type (state-calc gstate))))
    (list
     (update-widget 'text-view (get-id (string-append prepend "amount-value")) 'text
                    (string-append (number->string (convert-output amount (get-units))) " " (get-units)))
     (update-widget 'text-view (get-id (string-append prepend "na"))
                    'text (number->string (convert-output (list-ref amounts 0) "kg/ha")))
     (update-widget 'text-view (get-id (string-append prepend "pa"))
                    'text (number->string (convert-output (list-ref amounts 1) "kg/ha")))
     (update-widget 'text-view (get-id (string-append prepend "ka"))
                    'text (number->string (convert-output (list-ref amounts 2) "kg/ha")))
     ;; costs
     (update-widget 'text-view (get-id (string-append prepend "costn"))
                    'text (get-cost-string-from-nutrient 0 amounts 1))
     (update-widget 'text-view (get-id (string-append prepend "costp"))
                    'text (get-cost-string-from-nutrient 1 amounts 1))
     (update-widget 'text-view (get-id (string-append prepend "costk"))
                    'text (get-cost-string-from-nutrient 2 amounts 1))
     )))

(define (spacer size)
  (space (layout 'fill-parent size 1 'left 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (let ((units (if (equal? (current-units) metric)
                   "Kg/hectare"
                   "units/acre"))
        (a (if (equal? (current-units) metric) 100 (convert-output 200 "kg/ha")))
        (b (if (equal? (current-units) metric) 150 (convert-output 150 "kg/ha")))
        (c (if (equal? (current-units) metric) 100 (convert-output 100 "kg/ha")))
        (d (if (equal? (current-units) metric) 50 (convert-output 50 "kg/ha"))))
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

