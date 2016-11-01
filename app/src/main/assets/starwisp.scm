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

(msg "starwisp.scm")

(setup-database!)

;;(set-current! 'user-id (get-setting-value "user-id"))
;;(set! i18n-lang (get-setting-value "language"))

(define centre-layout (layout 'wrap-content 'wrap-content 1 'centre 0))

(define-activity-list

  (activity
   "splash"
   (vert
    (mtitle 'title)
    (mtext 'splash-about)
    (spacer 20)
    (mtext 'splash-blurb)
    (spacer 20)
    (mbutton 'splash-start 
	     (lambda () (list (start-activity-goto "main" 2 ""))))
    (spacer 20)
    (mtext 'splash-discl)
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
    (mtitle 'title)
    (build-drawmap (make-id "fieldmap") "readonly" fillwrap
		   (lambda (id) 
		     (msg "map button returned" id)
		     (list (start-activity "field" 2 id))))

    (scroll-view-vert
     0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
     (list
      (vert
      (mbutton 'calculator (lambda () (list (start-activity "calc" 2 ""))))
      (build-list-widget db "farm" 'fields (list "name") "field" "field"
			 (lambda () #f)
			 (lambda ()
			   '(("name" "varchar" "None")
			     ("soil" "varchar" "None")
			     ("crop" "varchar" "None")
			     ("previous-crop" "varchar" "None")
			     ("soil-test-n" "varchar" 0)
			     ("soil-test-p" "varchar" 0)
			     ("regularly-manure" "varchar" 0)
			     ("recently-grown-grass" "varchar" 0)
			     ("size" "real" 0))))

      (spacer 20)
      (mtoggle-button 
       'fertiliser-costs 
       (lambda (v) (list (update-widget 'linear-layout (get-id "costs-list") (if (zero? v) 'hide 'show) 0))))
      (linear-layout
       (make-id "costs-list")
       'vertical
       (layout 'fill-parent 'wrap-content 1 'centre 20)
       list-colour
       (list
	(mtext 'costs-blurb)
	(medit-text-scale 'n-cost "numeric" (lambda (v) '()))
	(medit-text-scale 'p-cost "numeric" (lambda (v) '()))
	(medit-text-scale 'k-cost "numeric" (lambda (v) '()))
	))
      (mtoggle-button 
       'custom-manures 
       (lambda (v) (list (update-widget 'linear-layout (get-id "custom-manures-list") (if (zero? v) 'hide 'show) 0))))
      (linear-layout
       (make-id "custom-manures-list")
       'vertical
       (layout 'fill-parent 'wrap-content 1 'centre 20)
       list-colour
       (list
	(mtext 'manures-blurb)
	(build-list-widget db "farm" 'custom-manures (list "name") "manure" "manure"
			   (lambda () #f)
			   (lambda ()
			     '(("name" "varchar" "None")
			       ("N" "real" 0)
			       ("P" "real" 0)
			       ("K" "real" 0))))
	))
      (spacer 20)
      (horiz
       (mspinner 'choose-units units-list
		 (lambda (v) (mutate-units! (list-ref units-list v)) '()))
       (mspinner 'rainfall rainfall-list
		 (lambda (v) '())))
      (mbutton 'email (lambda () (list (start-activity "email" 2 ""))))
      (mbutton 'about (lambda () (list (start-activity "about" 2 ""))))))))
     
   
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (let ((polygons (get-polygons)))
       (let ((centre (get-farm-centre polygons)))
	 (list
	  (update-widget 'linear-layout (get-id "costs-list") 'hide 1)
	  (update-widget 'linear-layout (get-id "custom-manures-list") 'hide 1)
	  (update-widget 'edit-text (get-id "n-cost") 'text (number->string (list-ref costs 0)))
	  (update-widget 'edit-text (get-id "p-cost") 'text (number->string (list-ref costs 1)))
	  (update-widget 'edit-text (get-id "k-cost") 'text (number->string (list-ref costs 2)))
	  (update-widget 'draw-map (get-id "fieldmap") 'polygons (list "none highlighted" (get-polygons)))
	  (update-widget 'draw-map (get-id "fieldmap") 'centre (list (vx centre) (vy centre) 15))
	  (update-list-widget db "farm" (list "name") "field" "field" #f)
	  (update-list-widget db "farm" (list "name") "manure" "manure" #f)
	  (update-widget 'spinner (get-id "choose-units-spinner") 'selection
			 (if (eq? (current-units) 'metric) 0 1))
	  ))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))
  
  
  (activity
   "calc"
    (scroll-view-vert
     0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
     (list

      (vert
       (mtitle 'crap-calculator)
       (horiz
	(calc-manure-type-widget (lambda (v)))
	(mspinner 
	 'application-type fym-application-list 
	 (lambda (v) '())))

       (horiz
	(mspinner 
	 'soil-type soil-type-list 
	 (lambda (v) (update-soil! (list-ref soil-type-list v))))

	(mspinner 
	 'crop-type crop-type-list 
	 (lambda (v) (update-crop! (list-ref crop-type-list v)))))

       (horiz
	(mspinner 'season season-list (lambda (v) (update-season! (list-ref season-list v))))
	(calc-manure-quality-widget (lambda (v))))

       (calc-amount-widget (lambda (v)))
       
       (text-view (make-id "amount-value") "4500 gallons" 30
		  (layout 'wrap-content 'wrap-content 1 'centre 0))
       (spacer 10)
       (calc-results)
       (mbutton 'done (lambda () (list (finish-activity 99)))))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (if (equal? (current-units) 'metric) 
	 '()
	 (list
	  (update-widget 'text-view (get-id "nutrient-n-metric") 'text (mtext-lookup 'nutrient-n-imperial))
	  (update-widget 'text-view (get-id "nutrient-p-metric") 'text (mtext-lookup 'nutrient-p-imperial))
	  (update-widget 'text-view (get-id "nutrient-k-metric") 'text (mtext-lookup 'nutrient-k-imperial)))))     
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  
  (activity
   "field"
   (vert
    (build-drawmap (make-id "map") "edit" fillwrap  
		   (lambda (polygon) 
		     ;; delete previous polygon??...
		     (index-for-each 
		      (lambda (i coord)
			(entity-init&save! 
			 db "farm" "coord"
			 (list
			  (ktv "name" "varchar" "") ;; argh
			  (ktv "parent" "varchar" (get-current 'field-id #f)) 
			  (ktv "order" "int" i) 
			  (ktv "lat" "real" (list-ref coord 0))
			  (ktv "lng" "real" (list-ref coord 1)))))
		      polygon)
		     ;; reset the field again
		     (entity-init! 
		      db "farm" "field" 
		      (get-entity-by-unique 
		       db "farm" (get-current 'field-id #f)))
		     ;; update area calculation
		     (entity-update-single-value! 
		      (ktv "size" "real" (area-metres polygon)))
		     (list
		      (mupdate 'edit-text 'field-size "size"))))

    (scroll-view-vert
     0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
     (list
      (vert
    
    (horiz
     (medit-text-scale 'field-name "normal" 
		       (lambda (v) (entity-update-single-value! 
				    (ktv "name" "varchar" v)) '()))
     (medit-text-scale 'field-size "numeric" 
		       (lambda (v)
			 (entity-update-single-value! 
			  (ktv "size" "real" (safe-string->number v))) '())))
    
    (canvas (make-id "graph")
	    (layout 'fill-parent 250 1 'centre 0)
	    (list))

    (build-list-widget db "farm" 'events (list "date" "type") "event" "fieldcalc"
		       (lambda () (get-current 'field-id #f))
		       (lambda ()
			 (list '("name" "varchar" "None")
			       '("parent" "varchar" "")
			       '("type" "varchar" "cattle")
			       (list "date" "varchar" (date->string (current-date)))
			       '("nutrients-n" "real" 0)
			       '("nutrients-p" "real" 0)
			       '("nutrients-k" "real" 0)
			       '("soil" "varchar" "normal")
			       '("size" "real" 0)
			       '("amount" "real" 0)
			       '("quality" "varchar" "DM2")
			       '("season" "varchar" "winter")
			       '("crop" "varchar" "normal"))))

    (vert-colour
     list-colour
     (mtitle 'crop-info)
     (horiz
      (mspinner 'crop-type crop-type-list
		(lambda (v) (entity-update-single-value! 
			     (ktv "crop" "varchar" 
				  (spinner-choice crop-type-list v))) '()))
      (mspinner 
       'grown-grass yesno-list 
       (lambda (v) 
	 (entity-update-single-value! 
	  (ktv "recently-grown-grass" "varchar" 
	       (spinner-choice yesno-list v)))'())))
     (mspinner 
      'previous-crop-type previous-crop-type-list 
      (lambda (v) 
	(entity-update-single-value! 
	 (ktv "previous-crop" "varchar" 
	     (spinner-choice previous-crop-type-list v)))
	'())))

    (vert-colour 
     list-colour
     (mtitle 'soil-info)
     (mspinner 
      'soil-type soil-type-list
      (lambda (v) (entity-update-single-value! 
		   (ktv "soil" "varchar" 
			(spinner-choice soil-type-list v))) '())) 
     (mspinner 
      'regular-organic yesno-list 
      (lambda (v) 
	(entity-update-single-value! 
	 (ktv "regularly-manure" "varchar" 
	      (spinner-choice yesno-list v)))
	'()))    
     (mtext 'soil-test)
     (horiz
     (mspinner 
      'soil-test-n soil-test-n-list 
      (lambda (v) 
	(entity-update-single-value! 
	 (ktv "soil-test-n" "varchar" 
	      (spinner-choice soil-test-n-list v)))
	'()))
     (mspinner 
      'soil-test-p soil-test-p-list 
      (lambda (v) 
	(entity-update-single-value! 
	 (ktv "soil-test-p" "varchar" 
	      (spinner-choice soil-test-p-list v)))
	'()))))
     
     (spacer 20)
 
   
    (horiz
     (delete-button)
     (mbutton-scale 'back (lambda () (list (finish-activity 99)))))))))
   
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "farm" "field" (get-entity-by-unique db "farm" arg))
     (set-current! 'field-id arg)     
     (set-current! 'field-name (entity-get-value "name"))     
     (let ((polygons (get-polygons)))
       (let ((centre (get-farm-centre polygons)))
	 (list
	  (update-widget 'draw-map (get-id "map") 'polygons (list (entity-get-value "unique_id") (get-polygons)))
	  (update-widget 'draw-map (get-id "map") 'centre (list (vx centre) (vy centre) 15))
	  (mupdate 'edit-text 'field-name "name")
	  (update-list-widget db "farm" (list "date") "event" "eventview" (get-current 'field-id #f))
	  (mupdate-spinner 'soil-type "soil" soil-type-list)
	  (mupdate-spinner 'crop-type "crop" crop-type-list)
	  (mupdate 'edit-text 'field-size "size")
	  (update-widget 'canvas (get-id "graph") 'drawlist (build-graph))      
	  (mupdate-spinner 'previous-crop-type "previous-crop" previous-crop-type-list)
	  (mupdate-spinner 'soil-test-n "soil-test-n" soil-test-n-list)
	  (mupdate-spinner 'soil-test-p "soil-test-p" soil-test-p-list)
	  (mupdate-spinner 'regular-organic "regularly-manure" yesno-list)
	  (mupdate-spinner 'grown-grass "recently-grown-grass" yesno-list)
	  ))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


;------------------------

  (activity
   "fieldcalc"
   (scroll-view-vert
    0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
    (list
     (vert
      (text-view (make-id "title") "field name" 40 fillwrap)
      (mtext 'field-calc-blurb) 
      (horiz
       (calc-manure-type-widget 
	(lambda (v)
	  (entity-set-value! "type" "varchar" (symbol->string v))))
       (vert
	(mtext 'date-text)
	(mbutton
	 'date
	 (lambda ()
	   (list (date-picker-dialog
		  "fieldcalc-date"
		  (lambda (day month year)
		    (update-calc!
		     (lambda (c)
		       (calc-modify-date c (list day (+ month 1) year))))
		    ;; updating the calculator and the database...
		    (entity-set-value! "date" "varchar" (date->string (current-date)))
		    (entity-set-value! "season" "varchar" (symbol->string (date->season (current-date))))
		    (append
		     (update-season! (date->season (current-date)))
		     (list
		      (update-widget 'button (make-id "date") 'text
				     (date->string (list day (+ month 1) year))))))))))))
       
      (horiz
       (calc-manure-quality-widget
	(lambda (v)
	  (entity-set-value! "quality" "varchar" (symbol->string v))))
       (mspinner 
	'application-type fym-application-list 
	(lambda (v) '())))
      
      (calc-amount-widget
       (lambda (v) 
	 (let ((amount (convert-input (* (current-seek-mul) v) (get-units))))
	   (msg "setting amount" amount)
	   (entity-set-value! "amount" "real" amount))))
      
      (text-view (make-id "amount-value") "4500 gallons" 30
		 (layout 'wrap-content 'wrap-content 1 'centre 0))
      (spacer 10)
      (calc-results)
      (horiz
       (mbutton-scale 
	'save 
	(lambda () 
	  ;; update nutrient values now...
	  (let ((nutrients (calc-nutrients)))
	    ;; need to convert output here??
	    (entity-set-value! "nutrients-n" "real" (list-ref nutrients 0))
	    (entity-set-value! "nutrients-p" "real" (list-ref nutrients 1))
	    (entity-set-value! "nutrients-k" "real" (list-ref nutrients 2)))	  
	  (entity-update-values!) 
	  (list (finish-activity 0))))
       (mbutton-scale 'cancel (lambda () (list (finish-activity 0)))))
      )))
     
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "farm" "event" (get-entity-by-unique db "farm" arg))
     (set-current! 'event-id arg)
     ;; update the calculator with values from the current field
     (let ((field (get-entity-by-unique db "farm" (entity-get-value "parent"))))
       (update-crop! (string->symbol (ktv-get field "crop")))
       (update-soil! (string->symbol (ktv-get field "soil")))
       ;; fill in the field related items here
       (entity-set-value! "soil" "varchar" (ktv-get field "soil"))
       (entity-set-value! "size" "real" (ktv-get field "size"))
       (list
	(update-widget 'button (make-id "date") 'text (entity-get-value "date"))
	(update-widget 'text-view (make-id "title") 'text (ktv-get field "name"))
	(update-widget 'image-view (get-id "example") 'image
		       (find-image (string->symbol (entity-get-value "type"))
				   (entity-get-value "amount")))
	)))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))
  
  (activity
   "eventview"     
   (vert
    (mtext 'title)
    (event-view-item "type" 'report-type)
    (event-view-item "date" 'report-date)
    (event-view-item "amount" 'report-amount)
    (event-view-item "quality" 'report-quality)
    (event-view-item "season" 'report-season)
    (event-view-item "crop" 'report-crop)
    (event-view-item "soil" 'report-soil)
    (event-view-item "size" 'report-size)
    ;; (event-view-item "total" 'report-total) size * amount
    (calc-results)
    (spacer 10)
    (mbutton-scale 
     'camera
     (lambda ()
       (list (start-activity "camera" 2 ""))))
    (spacer 10)
    (calc-gallery)
    (spacer 10)
    (horiz
     (delete-button)
     (mbutton-scale 'back (lambda () (list (finish-activity 0)))))

    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "farm" "event" (get-entity-by-unique db "farm" arg))
     (set-current! 'event-id arg)
     ;; update the calculator with values from the current field
     (append
      (if (eq? (current-units) 'metric) (list)
          (list
           (update-widget 'text-view (get-id "nt") 'text "N units/acre")
           (update-widget 'text-view (get-id "pt") 'text "P units/acre")
           (update-widget 'text-view (get-id "kt") 'text "K units/acre")))
     
      (let ((field (get-entity-by-unique db "farm" (entity-get-value "parent")))
	    (amounts (list (entity-get-value "nutrients-n")
			   (entity-get-value "nutrients-p")
			   (entity-get-value "nutrients-k"))))
	(list
	 (update-widget 'text-view (make-id "title") 'text (ktv-get field "name"))
	 (update-event-view-item-lookup "type")
	 (update-event-view-item "date")
	 (update-event-view-item "amount")
	 (update-event-view-item-lookup "quality")
	 (update-event-view-item-lookup "season")
	 (update-event-view-item-lookup "crop")
	 (update-event-view-item-lookup "soil")
	 (update-event-view-item "size")
 	 (update-widget 'text-view (get-id "na") 'text (convert-output (entity-get-value "nutrients-n") "kg/ha"))
	 (update-widget 'text-view (get-id "pa") 'text (convert-output (entity-get-value "nutrients-p") "kg/ha"))
	 (update-widget 'text-view (get-id "ka") 'text (convert-output (entity-get-value "nutrients-k") "kg/ha"))
	 (update-widget 'text-view (get-id "costn") 'text (get-cost-string-from-nutrient 0 amounts (entity-get-value "size")))
	 (update-widget 'text-view (get-id "costp") 'text (get-cost-string-from-nutrient 1 amounts (entity-get-value "size")))
	 (update-widget 'text-view (get-id "costk") 'text (get-cost-string-from-nutrient 2 amounts (entity-get-value "size")))
	 
	 ))))
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
                           (get-current 'field-name #f)
                           "-"
                           (date->path (string->date (entity-get-value "date")))
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
                     (find-image (string->symbol (entity-get-value "type"))
                                 (entity-get-value "amount")))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))
  
  (activity
   "manure"
   (scroll-view-vert
    0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
    (list
     (vert
      (medit-text 'manure-name "normal" (lambda (v) (entity-update-single-value! (ktv "name" "varchar" v)) '()))
      (medit-text 'manure-n "numeric" (lambda (v) (entity-update-single-value! (ktv "N" "real" (safe-string->number v))) '()))
      (medit-text 'manure-p "numeric" (lambda (v) (entity-update-single-value! (ktv "P" "real" (safe-string->number v))) '()))
      (medit-text 'manure-k "numeric" (lambda (v) (entity-update-single-value! (ktv "K" "real" (safe-string->number v))) '()))
      (horiz
       (delete-button)
       (mbutton-scale 'back (lambda () (list (finish-activity 99))))))))
   
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "farm" "manure" (get-entity-by-unique db "farm" arg))
     (list
      (mupdate 'edit-text 'manure-name "name")
      (mupdate 'edit-text 'manure-n "N")
      (mupdate 'edit-text 'manure-p "P")
      (mupdate 'edit-text 'manure-k "K")
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))



  )
