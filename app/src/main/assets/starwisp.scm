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
  (if (and (equal? (current-units) imperial)
           (or (equal? manure cattle)
               (equal? manure pig)))
      (mutate-current-seek-mul! 100)
      (cond
       ((equal? manure poultry)
        (if (equal? (current-units) imperial)
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
            (list (start-activity "field" 2 (field-name field))))))
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
  (cond
   ((or
     (eqv? (list-ref d 1) 2)
     (eqv? (list-ref d 1) 3)
     (eqv? (list-ref d 1) 4)) spring)
   ((or
     (eqv? (list-ref d 1) 5)
     (eqv? (list-ref d 1) 6)
     (eqv? (list-ref d 1) 7)) summer)
   ((or
     (eqv? (list-ref d 1) 8)
     (eqv? (list-ref d 1) 9)
     (eqv? (list-ref d 1) 10)) autumn)
   ((or
     (eqv? (list-ref d 1) 11)
     (eqv? (list-ref d 1) 12)
     (eqv? (list-ref d 1) 1)) winter)))

(define centre-layout (layout 'wrap-content 'wrap-content 1 'centre 0))

(define-activity-list

  (activity
   "splash"
   (vert
    (text-view (make-id "splash-title") "The Farm Crap App" 40 centre-layout)
    (text-view (make-id "splash-about") "Manage your muck with the Farm Crap App" 20 centre-layout)
    (spacer 20)
    (text-view (make-id "splash-blurb") "Developed by <a href='http://fo.am/kernow'>FoAM Kernow</a> on behalf of the <a href='www.swarmhub.co.uk'>SWARM Knowledge Hub</a>, a Rural Development Programme for England (RDPE) initiative managed by <a href='http://www.duchy.ac.uk/'>Duchy College Rural Business School</a>, in partnership with Rothamsted Research North Wyke." 15 centre-layout)
    (spacer 20)
    (button (make-id "f2") "Get started!" 20 fillwrap
            (lambda () (list (start-activity-goto "main" 2 ""))))
    (spacer 20)
    (text-view (make-id "splash-discl") "The Farm Crap App offers information for guidance purposes only and is not intended to amount to professional advice or opinion. FoAM Kernow, Duchy College, and Rothamsted Research North Wyke cannot be held responsible for any losses or damage resulting from the use of information provided by this app." 15 centre-layout)
    (image-view (make-id "about-logo") "logo" fillwrap)
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "main"
   (vert
    (text-view (make-id "title") "Farm Crap App Pro" 40 fillwrap)
    (text-view (make-id "title") "Your fields" 30 fillwrap)
    (build-drawmap-fragment (make-id "map") fillwrap)

    (linear-layout
     (make-id "main-field-list")
     'vertical
     (layout 'fill-parent 'fill-parent 1 'left 0)
     (list 0 0 0 0)
     (build-field-buttons))
    (spacer 20)
;    (button (make-id "f3") "New field" 20 fillwrap
;            (lambda ()
;              (list
;               (start-activity "newfield" 2 ""))))
    (text-view (make-id "measure-text") "Measurement units" 20 fillwrap)
    (spinner (make-id "units") (list metric imperial) fillwrap
             (lambda (v)
               (mutate-units! (list-ref (list metric imperial) v))
               (list)))
    (spacer 20)
    (button (make-id "f2") "Calculator" 20 fillwrap
            (lambda () (list (start-activity "calc" 2 ""))))
    (button (make-id "email-button") "Export" 20 fillwrap
            (lambda ()
              (list
               (start-activity "email" 2 ""))))
    (button (make-id "about-button") "About" 20 fillwrap
            (lambda ()
              (list
               (start-activity "about" 2 "")))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget 'spinner (get-id "units") 'selection
                     (if (equal? (current-units) metric) 0 1))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode)
     (list
      (update-widget 'linear-layout (get-id "main-field-list") 'contents
                     (build-field-buttons)))))

  (activity
   "email"
   (vert
    (text-view (make-id "title") "Export" 40 fillwrap)
    (text-view (make-id "measure-text") "Email export all field data" 20 fillwrap)
    (button (make-id "email-button") "Send Email" 20 fillwrap
            (lambda ()
              (save-data "fields.csv" (stringify-fields))
              (list
               (send-mail "" "From your Crap Calculator"
                          "Please find attached your field data."
                          (list (string-append dirname "fields.csv"))))))
    (button (make-id "finished") "Done" 20 fillwrap
            (lambda () (list (finish-activity 99)))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget 'edit-text (get-id "email") 'text (current-email))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "calc"
   (vert
    (text-view (make-id "title") "Crap Calculator" 40 fillwrap)

    (text-view (make-id "manure-text") "Manure type" 15 fillwrap)
    (spinner (make-id "manure") (list cattle FYM pig poultry) fillwrap
             (lambda (v)
	       (let ((v (list-ref (list cattle FYM pig poultry) v)))
		 (update-seek-mul! v)
		 (append
		  (update-type! "c" v)
		  (update-amount! "c" (convert-input (* (current-seek-mul) 50) (get-units)))
		  (list
		   (update-widget 'seek-bar (get-id "amount") 'init 0)
		   (update-widget 'spinner (get-id "cquality") 'array
				  (cond
				   ((equal? v cattle) (list DM2 DM6 DM10))
				   ((equal? v pig) (list DM2 DM4-pig DM6-pig))
				   ((equal? v poultry) (list layer broiler))
				   ((equal? v FYM) (list fresh other other1 other2))))
		   
		   (update-widget 'image-view (get-id "example") 'image
				  (find-image (calc-type (current-calc))
					      (calc-amount (current-calc)))))))))
	     
    (horiz
     (vert
      (text-view (make-id "soil-text") "Soil type" 15 fillwrap)
      (spinner (make-id "soil") (list sandyshallow mediumheavy) fillwrap
               (lambda (v) (update-soil! "c" (list-ref (list sandyshallow mediumheavy) v)))))
     (vert
      (text-view (make-id "crop-text") "Crop type" 15 fillwrap)
      (spinner (make-id "crop") (list normal grass-oilseed) fillwrap
               (lambda (v) (update-crop! "c" (list-ref (list normal grass-oilseed) v))))))

    (horiz
     (vert
      (text-view (make-id "season-text") "Season" 15 fillwrap)
      (spinner (make-id "season") (list autumn winter spring summer) fillwrap
               (lambda (v) (update-season! "c" (list-ref (list autumn winter spring summer) v)))))
     (vert
      (text-view (make-id "quality-text") "Quality" 15 fillwrap)
      (spinner (make-id "cquality") (list DM2 DM4 DM6) fillwrap
               (lambda (v)
		 (let ((v (list-ref (list DM2 DM4 DM6) v)))
		   (update-quality!
		    "c"
		    (cond
		     ((equal? v other1) other)
		     ((equal? v other2) other)
		     (else v))))))))

    (text-view (make-id "amount-text") "Application Rate" 15 fillwrap)
    (seek-bar (make-id "amount") 100 fillwrap
              (lambda (v)
                (append
                 (update-amount! "c" (convert-input (* (current-seek-mul) v) (get-units)))
                 (list
                  (update-widget 'image-view (get-id "example") 'image
                                 (find-image (calc-type (current-calc))
                                             (calc-amount (current-calc))))))))

    (text-view (make-id "camount-value") "4500 gallons" 30
               (layout 'wrap-content 'wrap-content 1 'centre 0))
    (spacer 10)

    (linear-layout
      (make-id "out-crop")
      'vertical
      (layout 'fill-parent 'wrap-content 1 'centre 0)
      (list 0 0 0 0)	     
      (list
       (text-view (make-id "out-crop-text") "Crop Available" 15 fillwrap)
       (linear-layout
        (make-id "h")
        'horizontal
        (layout 'fill-parent 'wrap-content 1 'centre 0)
	(list 0 0 0 0)
	(list
         (text-view (make-id "nt") "N Kg/ha" 20 wrap)
         (text-view (make-id "pt") "P Kg/ha" 20 wrap)
         (text-view (make-id "kt") "K Kg/ha" 20 wrap)))
       (horiz
        (text-view (make-id "cna") "12" 30
                   wrap)
        (text-view (make-id "cpa") "75" 30
                   wrap)
        (text-view (make-id "cka") "55" 30
                   wrap))))

    (linear-layout
     (make-id "out-cost")
     'vertical
     (layout 'fill-parent 'wrap-content 1 'centre 0)
     (list 0 0 0 0)
     (list
      (text-view (make-id "cost-text") "Fertiliser Savings (as at Oct 2013)" 15 fillwrap)
      (linear-layout
       (make-id "h")
       'horizontal
       (layout 'fill-parent 'wrap-content 1 'centre 0)
       (list 0 0 0 0)
       (list

        (space (layout 'wrap-content 'wrap-content 0.9 'centre 0))

        (linear-layout
         (make-id "h") 'horizontal (layout 'wrap-content 'wrap-content 0.2 'centre 0)
	 (list 0 0 0 0)
         (list
          (image-view (make-id "imn") "pound" wrap)
          (text-view (make-id "ccostn") "12" 25 wrap)))

        (space (layout 'wrap-content 'wrap-content 2 'centre 0))

        (linear-layout
         (make-id "h") 'horizontal (layout 'wrap-content 'wrap-content 0.2 'centre 0)
	 (list 0 0 0 0)
         (list
          (image-view (make-id "imp") "pound" wrap)
          (text-view (make-id "ccostp") "75" 25 wrap)))

        (space (layout 'wrap-content 'wrap-content 2 'centre 0))

        (linear-layout
         (make-id "h") 'horizontal (layout 'wrap-content 'wrap-content 0.2 'centre 0)
	 (list 0 0 0 0)
         (list
          (image-view (make-id "imk") "pound" wrap)
          (text-view (make-id "ccostk") "55" 25 wrap)))

        (space (layout 'wrap-content 'wrap-content 0.9 'centre 0))

        ))))

    (spacer 10)
    (image-view (make-id "example") "test" (layout 'fill-parent 'fill-parent 1 'centre 0))
    (spacer 10)

    (button (make-id "finished") "Done" 20 fillwrap
            (lambda () (list (finish-activity 99)))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (append
      (list
 ;      (update-widget 'linear-layout (get-id "out-crop") 'show 0)
 ;      (update-widget 'linear-layout (get-id "out-cost") 'hide 0)
       (update-widget 'seek-bar (get-id "amount") 'init 0)
       (update-widget 'image-view (get-id "example") 'image
                      (find-image (calc-type (current-calc))
                                  (calc-amount (current-calc)))))
      (update-amount! "c" (convert-input (* (current-seek-mul) 50) (get-units)))
      (if (equal? (current-units) metric) (list)
          (list
           (update-widget 'text-view (get-id "nt") 'text "N units/acre")
           (update-widget 'text-view (get-id "pt") 'text "P units/acre")
           (update-widget 'text-view (get-id "kt") 'text "K units/acre")))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "newfield"
    (vert
     (text-view (make-id "title") "Make a new field" 40 fillwrap)
     (text-view (make-id "name-txt") "Field name" 15 fillwrap)

     (edit-text (make-id "name") "" 20 "text" fillwrap
                (lambda (v)
                  (mutate-state!
                   (lambda (s)
                     (state-modify-field
                      s (field-modify-name (state-field s) v))))
                  '()))
     (text-view (make-id "soil-text") "Soil type" 15 fillwrap)
     (spinner (make-id "soil") (list sandyshallow mediumheavy) fillwrap
              (lambda (v)
                (mutate-state!
                 (lambda (s)
                   (state-modify-field
                    s (field-modify-soil (state-field s) 
					 (list-ref (list sandyshallow mediumheavy) v)))))
                '()))
     (text-view (make-id "crop-text") "Crop type" 15 fillwrap)
     (spinner (make-id "crop") (list normal grass-oilseed) fillwrap
              (lambda (v)
                (mutate-state!
                 (lambda (s)
                   (state-modify-field
                    s (field-modify-crop (state-field s) 
					 (list-ref (list normal grass-oilseed) v)))))
                '()))

     (text-view (make-id "field-size-text")
                (string-append
                 "Field size in "
                 (if (equal? (current-units) metric) "hectares" "acres"))
                15 fillwrap)

     (edit-text (make-id "field-size") "0" 20 "numeric" fillwrap
                (lambda (v)
                  (mutate-state!
                   (lambda (s)
                     (state-modify-field
                      s (field-modify-size
                         (state-field s)
                         (dbg (convert-input (dbg (string->number v)) "hectares"))))))
                  '()))

     (horiz
      (button (make-id "save") "Save" 20 fillwrap
              (lambda ()
                (mutate-saved-data!
                 (lambda (d)
                   (saved-data-modify-fields
                    d (append (saved-data-fields d) (list (current-field))))))
                (list (finish-activity 99))))
      (button (make-id "cancel") "Cancel" 20 fillwrap
              (lambda () (list (finish-activity 99)))))
     )

    (lambda (activity arg)
      (activity-layout activity))
    (lambda (activity arg)
      (list
       (update-widget 'text-view (get-id "field-size-text") 'text
                      (string-append
                       "Field size in "
                       (if (equal? (current-units) metric)
                           "hectares" "acres")))))
    (lambda (activity) '())
    (lambda (activity) '())
    (lambda (activity) '())
    (lambda (activity) '())
    (lambda (activity requestcode resultcode) '()))

  (activity
   "field"
    (vert
     (text-view (make-id "field-title") "Long Meadow" 40 fillwrap)
     (canvas (make-id "graph")
             (layout 'fill-parent 250 1 'centre 0)
             (list))
     (text-view (make-id "events-txt") "Events" 20 fillwrap)
     (linear-layout
      (make-id "field-events-list")
      'vertical
      (layout 'fill-parent 'fill-parent 1 'left 0)
      (list 0 0 0 0)
      (build-event-buttons))
     (spacer 20)
     (button (make-id "event") "New spreading event" 20 fillwrap
             (lambda ()
               (list (start-activity "fieldcalc" 2 ""))))
     (spacer 20)
     (horiz
      (button (make-id "delete") "Delete" 20 (layout 'fill-parent 'wrap-content 0.7 'left 0)
              (lambda ()
                (list
                 (alert-dialog
                  "deleteme"
                  (string-append "Do you want to delete '" (field-name (current-field)) "'?")
                  (lambda (r)
                    (cond
                     ((zero? r) (list))
                     (else
                      (mutate-saved-data!
                       (lambda (d)
                         (saved-data-modify-fields
                          d (fields-remove-field
                             (get-fields)
                             (field-name (current-field))))))
                      (mutate-current-field! (lambda (f) (empty-field)))
                      (list (finish-activity 99)))))))))
      (button (make-id "back") "Back" 20 (layout 'fill-parent 'wrap-content 0.3 'left 0)
              (lambda ()
                (mutate-current-field! (lambda (f) (empty-field)))
                (list (finish-activity 99))))))

    (lambda (activity arg)
      (activity-layout activity))
    (lambda (activity arg)
      ;; load up into the current field
      (mutate-current-field! (lambda (f) (find-field arg)))
      (list
       (update-widget 'text-view (get-id "field-title") 'text arg)
       (update-widget 'linear-layout (get-id "field-events-list") 'contents
                      (build-event-buttons))
       (update-widget 'canvas (get-id "graph") 'drawlist (build-graph))))
    (lambda (activity) '())
    (lambda (activity) '())
    (lambda (activity) '())
    (lambda (activity) '())
    (lambda (activity requestcode resultcode)
      (list
       (update-widget 'linear-layout (get-id "field-events-list") 'contents
                      (build-event-buttons)))))

  (activity
   "fieldcalc"
    (vert
     (text-view (make-id "fieldcalc-title") "field name" 40 fillwrap)
     (text-view (make-id "blurb") "Enter new crap spreading event" 20 fillwrap)
     (horiz
      (text-view (make-id "fc-date-text")
                 (date->string (list date-day date-month date-year)) 25 fillwrap)
      (button (make-id "date") "Set date" 20 fillwrap
              (lambda ()
                (list (date-picker-dialog
                       "fieldcalc-date"
                       (lambda (day month year)
                         (mutate-state!
                          (lambda (s)
                            (state-modify-date s (list day (+ month 1) year))))
                         (update-season! "fc" (date->season (current-date)))
                         (list
                          (update-widget 'text-view (make-id "fc-date-text") 'text
                                         (date->string (list day (+ month 1) year))))))))))

     (horiz
      (vert
       (text-view (make-id "manure-text") "Manure type" 15 fillwrap)
       (spinner (make-id "manure") (list cattle FYM pig poultry) fillwrap
                (lambda (v)
		  (let ((v (list-ref (list cattle FYM pig poultry))))
		    (update-seek-mul! v)
		    (append
		     (update-type! "fc" v)
		     (update-amount! "fc" (convert-input (* (current-seek-mul) 50) (get-units)))
		     (list
		      (update-widget 'seek-bar (get-id "amount") 'init 0)
		      (update-widget 'image-view (get-id "example") 'image
				     (find-image (calc-type (current-calc))
						 (calc-amount (current-calc))))
		      (update-widget 'spinner (get-id "quality") 'array
				     (cond
				      ((equal? v cattle) (list DM2 DM6 DM10))
				      ((equal? v pig) (list DM2 DM4-pig DM6-pig))
				      ((equal? v poultry) (list layer broiler))
				      ((equal? v FYM) (list fresh other other1 other2))))))))))

      (vert
       (text-view (make-id "quality-text") "Quality" 15 fillwrap)
       (spinner (make-id "quality") (list DM2 DM4 DM6) fillwrap
                (lambda (v)
		  (let ((v (list-ref (list DM2 DM4 DM6) v)))
		    (update-quality!
		     "fc"
		     (cond
		      ((equal? v other1) other)
		      ((equal? v other2) other)
		      (else v))))))))
     (text-view (make-id "amount-text") "Application Rate" 15 fillwrap)
     (seek-bar (make-id "amount") 100 fillwrap
              (lambda (v)
                (cons
                 (update-widget 'image-view (get-id "example") 'image
                     (find-image (calc-type (current-calc))
                                 (calc-amount (current-calc))))
                 (update-amount! "fc" (convert-input (* (current-seek-mul) v) (get-units))))))
     (text-view (make-id "fcamount-value") "4500 gallons" 30
                (layout 'wrap-content 'wrap-content 1 'centre 0))


    (linear-layout
      (make-id "out-crop")
      'vertical
      (layout 'fill-parent 'wrap-content 1 'centre 0)
      (list 0 0 0 0)
      (list
       (text-view (make-id "out-crop-text") "Crop Available" 15 fillwrap)
       (linear-layout
        (make-id "h")
        'horizontal
        (layout 'fill-parent 'wrap-content 1 'centre 0)
	(list 0 0 0 0)
        (list
         (text-view (make-id "nt") "N Kg/ha" 20 wrap)
         (text-view (make-id "pt") "P Kg/ha" 20 wrap)
         (text-view (make-id "kt") "K Kg/ha" 20 wrap)))
       (horiz
        (text-view (make-id "fcna") "12" 30
                   wrap)
        (text-view (make-id "fcpa") "75" 30
                   wrap)
        (text-view (make-id "fcka") "55" 30
                   wrap))))


    (linear-layout
     (make-id "out-cost")
     'vertical
     (layout 'fill-parent 'wrap-content 1 'centre 0)
     (list 0 0 0 0)
     (list
      (text-view (make-id "cost-text") "Fertiliser Savings (as at Oct 2013)" 15 fillwrap)
      (linear-layout
       (make-id "h")
       'horizontal
       (list 0 0 0 0)
       (layout 'fill-parent 'wrap-content 1 'centre 0)
       (list

        (space (layout 'wrap-content 'wrap-content 0.9 'centre 0))

        (linear-layout
         (make-id "h") 'horizontal (layout 'wrap-content 'wrap-content 0.2 'centre 0)
	 (list 0 0 0 0)
         (list
          (image-view (make-id "imn") "pound" wrap)
          (text-view (make-id "fccostn") "12" 25 wrap)))

        (space (layout 'wrap-content 'wrap-content 2 'centre 0))

        (linear-layout
         (make-id "h") 'horizontal (layout 'wrap-content 'wrap-content 0.2 'centre 0)
	 (list 0 0 0 0)
         (list
          (image-view (make-id "imp") "pound" wrap)
          (text-view (make-id "fccostp") "75" 25 wrap)))

        (space (layout 'wrap-content 'wrap-content 2 'centre 0))

        (linear-layout
         (make-id "h") 'horizontal (layout 'wrap-content 'wrap-content 0.2 'centre 0)
	 (list 0 0 0 0)
         (list
          (image-view (make-id "imk") "pound" wrap)
          (text-view (make-id "fccostk") "55" 25 wrap)))

        (space (layout 'wrap-content 'wrap-content 0.9 'centre 0))

        ))))

    (spacer 10)
    (image-view (make-id "example") "test" (layout 'fill-parent 'fill-parent 1 'centre 0))



    (spacer 10)


;;     (button (make-id "camera") "Camera" 20 fillwrap
;;             (lambda () (list (start-activity "camera" 2 ""))))

     (spacer 10)

     (horiz
      (button (make-id "save") "Save" 20 fillwrap
              (lambda ()

                (let ((event-id (mutate-make-event-id!)))
                  (mutate-current-field!
                   (lambda (field)
                     (display "adding event")(newline)
                     (display (field-size field))(newline)
                     (field-add-event
                      field
                      (event
                       event-id
                       (calc-type (state-calc gstate))
                       (current-date)
                       (calc-nutrients)
                       (calc-amount (state-calc gstate))
                       (calc-quality (state-calc gstate))
                       (calc-season (state-calc gstate))
                       (calc-crop (state-calc gstate))
                       (calc-soil (state-calc gstate))
                       (field-size field)
                       metric)))))

                (mutate-saved-data!
                 (lambda (d)
                   (saved-data-modify-field
                    (lambda (field)
                      (current-field))
                      (field-name (current-field))
                    d)))

                (list (finish-activity 99))))
      (button (make-id "cancel") "Cancel" 20 fillwrap
              (lambda () (list (finish-activity 99))))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (mutate-state!
      (lambda (s)
        (state-modify-date s (list date-day date-month date-year))))
     (update-soil! "fc" (field-soil (current-field)))
     (update-crop! "fc" (field-crop (current-field)))
     (update-season! "fc" (date->season (current-date)))
     (append
      (if (equal? (current-units) metric) (list)
          (list
           (update-widget 'text-view (get-id "nt") 'text "N units/acre")
           (update-widget 'text-view (get-id "pt") 'text "P units/acre")
           (update-widget 'text-view (get-id "kt") 'text "K units/acre")))
      (list
       (update-widget 'seek-bar (get-id "amount") 'init 0)
       (update-widget 'image-view (get-id "example") 'image
                      (find-image (calc-type (current-calc))
                                  (calc-amount (current-calc))))
       (update-widget 'text-view (get-id "fc-date-text") 'text (date->string (list date-day date-month date-year)))
       (update-widget 'text-view (get-id "fieldcalc-title") 'text (field-name (current-field))))

      (update-amount! "fc" (convert-input (* (current-seek-mul) 50) (get-units)))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "eventview"
   (let ((item (lambda (id title)
                 (horiz
                  (text-view-left (make-id (string-append id "-text")) title 20
                                  (layout 'fill-parent 'wrap-content 0.7 'left 0))
                  (text-view-left (make-id id) "type" 30
                                  (layout 'fill-parent 'wrap-content 0.3 'left 0))))))
     (vert
    (text-view (make-id "fieldview-title") "field name" 40 fillwrap)

    (item "type" "Type")
    (item "date" "Date")
    (item "eventview-amount" "Application Rate")
    (item "quality" "Quality")
    (item "season" "Season")
    (item "crop" "Crop")
    (item "soil" "Soil")
    (item "size" "Size")
    (item "total-amount" "Total Amount")
    (spacer 20)
    (text-view (make-id "fieldview-ca") "Crop Available" 30 fillwrap)
    (horiz
     (text-view (make-id "nt") "N Kg/ha" 20 fillwrap)
     (text-view (make-id "pt") "P Kg/ha" 20 fillwrap)
     (text-view (make-id "kt") "K Kg/ha" 20 fillwrap))
     (horiz
      (text-view (make-id "fcna") "12" 30 fillwrap)
      (text-view (make-id "fcpa") "75" 30 fillwrap)
      (text-view (make-id "fcka") "55" 30 fillwrap))

     (spacer 20)
     (text-view (make-id "cost-text") "Field Fertiliser Savings" 30 fillwrap)
     (linear-layout
      (make-id "h")
      'horizontal
      (layout 'fill-parent 'wrap-content 1 'centre 0)
      (list 0 0 0 0)
      (list

       (space (layout 'wrap-content 'wrap-content 0.9 'centre 0))

       (linear-layout
        (make-id "h") 'horizontal (layout 'wrap-content 'wrap-content 0.2 'centre 0)
	(list 0 0 0 0)
        (list
         (image-view (make-id "imn") "pound" wrap)
         (text-view (make-id "fcn-cost") "12" 25 wrap)))

       (space (layout 'wrap-content 'wrap-content 2 'centre 0))

       (linear-layout
        (make-id "h") 'horizontal (layout 'wrap-content 'wrap-content 0.2 'centre 0)
	(list 0 0 0 0)
        (list
         (image-view (make-id "imp") "pound" wrap)
         (text-view (make-id "fcp-cost") "75" 25 wrap)))

        (space (layout 'wrap-content 'wrap-content 2 'centre 0))

        (linear-layout
         (make-id "h") 'horizontal (layout 'wrap-content 'wrap-content 0.2 'centre 0)
	 (list 0 0 0 0)
         (list
          (image-view (make-id "imk") "pound" wrap)
          (text-view (make-id "fck-cost") "55" 25 wrap)))

        (space (layout 'wrap-content 'wrap-content 0.9 'centre 0))
        ))
     (spacer 20)

     (text-view (make-id "cost-text") "Fertiliser Unit Price (as at Oct 2013)" 30 fillwrap)
     (linear-layout
      (make-id "h")
      'horizontal
      (layout 'fill-parent 'wrap-content 1 'centre 0)
      (list 0 0 0 0)
      (list

       (space (layout 'wrap-content 'wrap-content 0.9 'centre 0))

       (linear-layout
        (make-id "h") 'horizontal (layout 'wrap-content 'wrap-content 0.2 'centre 0)
	(list 0 0 0 0)
        (list
         (image-view (make-id "imn") "pound" wrap)
         (text-view (make-id "fcunit-n") (padcash->string (list-ref costs 0)) 25 wrap)))

       (space (layout 'wrap-content 'wrap-content 2 'centre 0))

       (linear-layout
        (make-id "h") 'horizontal (layout 'wrap-content 'wrap-content 0.2 'centre 0)
	(list 0 0 0 0)
        (list
         (image-view (make-id "imp") "pound" wrap)
         (text-view (make-id "fcunit-p") (padcash->string (list-ref costs 1)) 25 wrap)))

        (space (layout 'wrap-content 'wrap-content 2 'centre 0))

        (linear-layout
         (make-id "h") 'horizontal (layout 'wrap-content 'wrap-content 0.2 'centre 0)
	 (list 0 0 0 0)
         (list
          (image-view (make-id "imk") "pound" wrap)
          (text-view (make-id "fcunit-k") (padcash->string (list-ref costs 2)) 25 wrap)))

        (space (layout 'wrap-content 'wrap-content 0.9 'centre 0))
        ))

     (spacer 20)

     (linear-layout
      (make-id "gallery")
      'vertical
      (layout 'fill-parent 'fill-parent 1 'left 0)
      (list 0 0 0 0)
      (list
       (button (make-id "load-gallery") "Load Gallery" 20 fillwrap
               (lambda ()
                 (let ((path (string-append
                              (field-name (current-field)) "-"
                              (number->string (event-id (current-event))) "/")))
                   (list
                    (list-files
                     (string-append "filelister-" path)
                     path
                     (lambda (images)
                       (list
                        (update-widget
                         'linear-layout (get-id "gallery") 'contents
                         (cons
                          (text-view (make-id "temp") "Gallery" 30 fillwrap)
                          (foldl
                           (lambda (image r)
                             (append
                              (list (image-view (make-id image)
                                                (string-append dirname path image)
                                                (layout 'wrap-content 240 1 'left 0))
                                    (spacer 10))
                              r))
                           '()
                           images))))))))))))


     (button (make-id "camera") "Camera" 20 fillwrap
             (lambda ()
               (setup-for-picture-from-event)
               (list (start-activity "camera" 2 ""))))

     (horiz
      (button (make-id "delete") "Delete" 20 (layout 'fill-parent 'wrap-content 0.7 'left 0)
              (lambda ()
                (list
                 (alert-dialog
                  "deleteme-event"
                  (string-append "Do you want to delete this event?")
                  (lambda (r)
                    (cond
                     ((zero? r) (list))
                     (else
                      ;; modify current field
                      (mutate-current-field!
                       (lambda (field)
                         (field-remove-event
                          field
                          (event-id (current-event)))))

                      ;; stick it in saved data
                      (mutate-saved-data!
                       (lambda (d)
                         (saved-data-modify-field
                          (lambda (field)
                            (current-field))
                          (field-name (current-field))
                          d)))

                      ;; clean out current just in case
                      (mutate-current-event! (lambda (ev) (empty-event)))
                      (list (finish-activity 99)))))))))

      (button (make-id "back") "Back" 20 (layout 'fill-parent 'wrap-content 0.3 'left 0)
              (lambda () (list (finish-activity 99)))))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (append
      (if (equal? (current-units) metric) (list)
          (list
           (update-widget 'text-view (get-id "nt") 'text "N units/acre")
           (update-widget 'text-view (get-id "pt") 'text "P units/acre")
           (update-widget 'text-view (get-id "kt") 'text "K units/acre")))
      (let ((type (event-type (current-event))))
        (list
         (update-widget 'text-view (get-id "fcna") 'text (convert-output (list-ref (event-nutrients (current-event)) 0) "kg/ha"))
         (update-widget 'text-view (get-id "fcpa") 'text (convert-output (list-ref (event-nutrients (current-event)) 1) "kg/ha"))
         (update-widget 'text-view (get-id "fcka") 'text (convert-output (list-ref (event-nutrients (current-event)) 2) "kg/ha"))
         (update-widget 'text-view (get-id "type") 'text (event-type (current-event)))
         (update-widget 'text-view (get-id "date") 'text (date->string (event-date (current-event))))

         (update-widget 'text-view (get-id "fcn-cost") 'text (get-cost-string-from-nutrient 0 (event-nutrients (current-event)) (event-size (current-event))))
         (update-widget 'text-view (get-id "fcp-cost") 'text (get-cost-string-from-nutrient 1 (event-nutrients (current-event)) (event-size (current-event))))
         (update-widget 'text-view (get-id "fck-cost") 'text (get-cost-string-from-nutrient 2 (event-nutrients (current-event)) (event-size (current-event))))

         (update-widget 'text-view (get-id "eventview-amount") 'text
                        (string-append
                         (number->string (convert-output (event-amount (current-event))
                                                         (nutrient-units (current-event))))
                         " " (nutrient-units (current-event))))

         (let ((qunits (amount-units (current-event))))
           (update-widget 'text-view (get-id "total-amount") 'text
                          (string-append
                           (number->string (convert-output (* (event-size (current-event))
                                                              (event-amount (current-event))) qunits))
                           " " qunits)))

         (update-widget 'text-view (get-id "quality") 'text (event-quality (current-event)))
         (update-widget 'text-view (get-id "season") 'text (event-season (current-event)))
         (update-widget 'text-view (get-id "crop") 'text (event-crop (current-event)))
         (update-widget 'text-view (get-id "soil") 'text (event-soil (current-event)))
         (update-widget 'text-view (get-id "size") 'text
                        (string-append
                         (number->string (convert-output (event-size (current-event)) "hectares")) " "
                         (if (equal? (current-units) metric)
                             "hectares"
                             "acres")))
         (update-widget 'text-view (get-id "fieldview-title") 'text (field-name (current-field)))))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "camera"
   (horiz
    (vert
     (camera-preview (make-id "camera") (layout 'fill-parent 320 1 'left 0))
     (button (make-id "take-pic") "Take photo" 10 fillwrap
            (lambda ()
              (let ((path (string-append
                           (field-name (current-field))
                           "-"
                           (number->string (event-id (current-event)))
                           "/")))
                (list
                 (make-directory path)
                 (update-widget 'camera-preview (get-id "camera") 'take-picture path))))))
    (vert
     (image-view (make-id "example") "test" (layout 'fill-parent 320 1 'left 0))
     (button (make-id "back") "Back" 10 fillwrap
             (lambda ()
               (list
                (update-widget 'camera-preview (get-id "camera") 'shutdown 0)
                (finish-activity 99))))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget 'image-view (get-id "example") 'image
                     (find-image (calc-type (current-calc))
                                 (calc-amount (current-calc))))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "about"
   (vert
    (text-view (make-id "about-title") "About" 40 fillwrap)
    (text-view (make-id "about-text")
               "Welcome to the Farm Crap App designed to help farmers make the most of your manure (slurry, FYM and poultry litter). The app contains 3 components; the calculator, the image library (to which you can add your own photos), and the record sheets. The calculator will determine the amount of crop-available key nutrients (N, P & K) within the manure at different spreading rates helping you decide how much to spread in order to meet the crop requirements, and also what this looks like."
               15 fillwrap)
    (spacer 20)
    (text-view (make-id "about-text2")
               "The image library can be used as a visual reference guide to estimate the spreading rate of manure already applied to the field and therefore calculate the amount of crop available nutrients that have been applied."
               15 fillwrap)
    (spacer 20)
    (text-view (make-id "about-text3")
               "The Farm Crap App can also be used to keep records of spreading events per field, how much manure was spread and what this translates to in terms of applied nutrients. You can also upload a photo of the spreading event. The records can be emailed to you to be stored on your computer."
               15 fillwrap)
    (spacer 20)
    (text-view (make-id "about-text4")
               "The data for the Farm Crap App has been compiled from Defra RB209 Fertiliser Manual and the <a href='http://www.swarmhub.co.uk/think_manure.php?id=3121'>Think Manures handbook</a>. Fertiliser values are based on October 2013 prices taken from MANNER NPK."
               15 fillwrap)
    (spacer 20)
    (button (make-id "goto-nvz") "About Nitrate Vulnerable Zones" 20 fillwrap
            (lambda ()
              (list
               (start-activity "nvz" 1 ""))))
    (spacer 20)
    (button (make-id "back") "Back" 20 fillwrap
            (lambda ()
              (list (finish-activity 99)))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "nvz"
   (vert
    (text-view (make-id "about-title") "Farming in a NVZ" 40 fillwrap)
    (text-view (make-id "about-text")
               "If you farm land within an NVZ (Nitrate Vulnerable Zone) there are legal restrictions on when you can apply slurries and manures to the land and how much you can apply in a calendar year. To find the full list of compliance regulations please visit: <a href='www.gov.uk/nitrate-vulnerable-zones'>www.gov.uk/nitrate-vulnerable-zones</a>"
               15 fillwrap)
    (spacer 20)
    (text-view (make-id "about-text2")
               "<b>It is not permitted to spread organic manures to any field at a rate that would result in the TOTAL N supplied exceed 250kg N/ha in any rolling 12 month period.</b>"
               15 fillwrap)
    (spacer 20)
    (text-view (make-id "about-text3")
               "The Farm Crap App <b>does not</b> calculate total N supply but rather crop available N. If you farm land in an NVZ you must make sure you are fully aware of the compliance rules, including closed periods for applying organic manures, and total nitrogen supply. If you don’t comply with the NVZ rules, you may be prosecuted and fined."
               15 fillwrap)
    (spacer 20)
    (text-view (make-id "about-text4")
               "More information on NVZs can be found on the Swarm Hub <a href='http://www.swarmhub.co.uk/sub_waste.php?id=2584'>here</a>."
               15 fillwrap)
    (spacer 20)
    (button (make-id "back") "Back" 20 fillwrap
            (lambda ()
              (list
               (finish-activity 99)))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  )
