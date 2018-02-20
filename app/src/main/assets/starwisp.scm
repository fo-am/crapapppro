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

(define zoom-out 4)
(define zoom-in 16)

(define centre-layout (layout 'wrap-content 'wrap-content 1 'centre 0))

(define-activity-list

  (activity
   "splash"
   (scroll-view-vert
    0 (layout 'fill-parent 'wrap-content 0 'centre 0)
    (list
     (vert
      (mtitle 'title)
      (text-view (make-id "version")
		 (string-append "Version " app-version) 20 fillwrap)
      (mtext 'splash-about)
      (spacer 20)
      (mtext 'splash-blurb)
      (spacer 20)
      (mbutton 'splash-start 
	       (lambda () (list (start-activity-goto "main" 2 ""))))
      (spacer 20)
      (mtext 'splash-discl)
      (image-view (make-id "about-logo") "logo" fillwrap)
      )))
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
   (linear-layout
     (make-id "top")
     'vertical
     (layout 'fill-parent 'wrap-content 1 'centre 0)
     '(0 0 0 0)
     (list 
      (build-drawmap (make-id "fieldmap") "readonly" 
		     (layout 'fill-parent 'fill-parent 1.25 'centre 0)
		     (lambda (id) 
		       (list (start-activity "field" 2 id))))
      (vert
       (scroll-view-vert
	0 (layout 'fill-parent 'wrap-content -1 'centre 0)
	(list
	 (vert
	  (mbutton 'calculator (lambda () (list (start-activity "calc" 2 ""))))
	  (build-list-widget db "farm" 'fields (list "name") "field" "field"
			    (lambda () #f)
			    (lambda ()
			      '(("name" "varchar" "My field")
				("soil" "varchar" "None")
				("crop" "varchar" "None")
				("previous-crop" "varchar" "None")
				("soil-test-p" "varchar" "0")
				("soil-test-k" "varchar" "0")
				("regularly-manure" "varchar" "no")
				("recently-grown-grass" "varchar" "no")
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
	   (medit-text-scale 'n-cost "numeric" (lambda (v) (mutate-cost-n! v) '()))
	   (medit-text-scale 'p-cost "numeric" (lambda (v) (mutate-cost-p! v) '()))
	   (medit-text-scale 'k-cost "numeric" (lambda (v) (mutate-cost-k! v) '()))
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
				'(("name" "varchar" "My manure")
				  ("N" "real" 0)
				  ("P" "real" 0)
				  ("K" "real" 0))))
	   ))
	 (spacer 20)
	 (horiz
	  (mspinner 'choose-units units-list
		    (lambda (v) 
		      (mutate-units! (list-ref units-list v)) 
		      (if (equal? (current-units) 'metric) 
			  (list
			   (update-widget 'text-view (get-id "n-cost-text") 'text (mtext-lookup 'n-cost))
			   (update-widget 'text-view (get-id "p-cost-text") 'text (mtext-lookup 'p-cost))
			   (update-widget 'text-view (get-id "k-cost-text") 'text (mtext-lookup 'k-cost)))
			  (list
			   (update-widget 'text-view (get-id "n-cost-text") 'text (mtext-lookup 'n-cost-imperial))
			   (update-widget 'text-view (get-id "p-cost-text") 'text (mtext-lookup 'p-cost-imperial))
			   (update-widget 'text-view (get-id "k-cost-text") 'text (mtext-lookup 'k-cost-imperial))))))
	  (mspinner 'rainfall rainfall-list
		    (lambda (v) (mutate-rainfall! (list-ref rainfall-list v)) '())))
	 (mbutton 'email (lambda () (list (start-activity "email" 2 ""))))
	 ;(mbutton 'about (lambda () (list (start-activity "about" 2 ""))))
	 ))))))
     
   
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (let ((polygons (get-polygons)))
       (let ((zoom (if (polygons-empty? polygons) zoom-out zoom-in))
	     (centre (get-farm-centre polygons)))
	 (append
	  (if (equal? (current-units) 'metric) 
	      '()
	      (list
	       (update-widget 'text-view (get-id "n-cost-text") 'text (mtext-lookup 'n-cost-imperial))
	       (update-widget 'text-view (get-id "p-cost-text") 'text (mtext-lookup 'p-cost-imperial))
	       (update-widget 'text-view (get-id "k-cost-text") 'text (mtext-lookup 'k-cost-imperial))))
	  (list	  
	   (update-widget 'toggle-button (get-id "custom-manures") 'checked 0) 
	   (update-widget 'linear-layout (get-id "costs-list") 'hide 1)
	   (update-widget 'linear-layout (get-id "custom-manures-list") 'hide 1)
	   (update-widget 'edit-text (get-id "n-cost") 'text (number->string (rounding-cash (current-cost-n))))
	   (update-widget 'edit-text (get-id "p-cost") 'text (number->string (rounding-cash (current-cost-p))))
	   (update-widget 'edit-text (get-id "k-cost") 'text (number->string (rounding-cash (current-cost-k))))
	   (update-widget 'draw-map (get-id "fieldmap") 'polygons (list "none highlighted" (get-polygons)))
	   (update-widget 'draw-map (get-id "fieldmap") 'centre (list (vx centre) (vy centre) zoom))
	   (update-list-widget db "farm" (list "name") "field" "field" #f)
	   (update-list-widget db "farm" (list "name") "manure" "manure" #f)
	   (update-widget 'spinner (get-id "choose-units-spinner") 'selection
			  (if (eq? (current-units) 'metric) 0 1))
	   (update-widget 'spinner (get-id "rainfall-spinner") 'selection
			  (index-find (current-rainfall) rainfall-list))
	   ;; updates for orientation change
	   (update-widget 'linear-layout (get-id "top") 'orientation (if (eq? screen-orientation 'portrait) 'vertical 'horizontal)) 
	   )))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) 
     (msg requestcode resultcode)
     '()))
  
  
  (activity
   "calc"
    (scroll-view-vert
     0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
     (list

      (vert
       (mtitle 'crap-calculator)
       (horiz
	(calc-manure-type-widget (lambda (v)))
	(calc-manure-application-widget (lambda (v))))

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
     (set-current! 'calc-mode 'calc)
     (clear-soil-test!) ;; remove last values from field
     (update-fieldsize! 1) ;; reset after any fields
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
    (linear-layout
     (make-id "top")
     'vertical
     (layout 'fill-parent 'wrap-content 1 'centre 0)
     '(0 0 0 0)
     (list 
      (build-drawmap (make-id "map") "edit" (layout 'fill-parent 'wrap-content 1 'centre 0)  
		     (lambda (polygon) 
		       ;; delete previous polygon if one exists
		       (db-delete-children db "farm" "coord" (get-current 'field-id #f))

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
		       (entity-init! db "farm" "field" 
				     (get-entity-by-unique db "farm" (get-current 'field-id #f)))
		       ;; update area calculation
		       (entity-update-single-value! 
			(ktv "size" "real" (m2->hectares (area-metres polygon))))
		       (list
			(update-widget 'edit-text (get-id "field-size") 'text 
				       (number->string (convert-output (entity-get-value "size") 
								       "hectares"))))))
      (vert
      (scroll-view-vert
       0 (layout 'fill-parent 'wrap-content 0 'centre 0)
       (list
	(vert
	 
	 (horiz
	  (medit-text-scale 'field-name "normal" 
			    (lambda (v) 
			      (entity-update-single-value! 
			       (ktv "name" "varchar" v)) 
			      ;; update the name on the map
			      (list (update-widget 'draw-map (get-id "map") 'update-name v))))
	  (medit-text-scale 'field-size "numeric" 
			    (lambda (v)
			      (entity-update-single-value! 
			       (ktv "size" "real" (convert-input (safe-string->number v) "hectares"))) '())))
	 
	 (build-list-widget db "farm" 'events (list "date" "type") "event" "fieldcalc"
			    (lambda () (get-current 'field-id #f))
			    (lambda ()
			      (list '("name" "varchar" "not used")
				    '("parent" "varchar" "")
				    '("type" "varchar" "cattle")
				    (list "date" "varchar" (date->string (current-date)))
				    '("nutrients-n" "real" 0)
				    '("nutrients-p" "real" 0)
				    '("nutrients-k" "real" 0)
				    '("total-nutrients-n" "real" 0)
				    '("total-nutrients-p" "real" 0)
				    '("total-nutrients-k" "real" 0)
				    '("require-n" "real" 0)
				    '("require-p" "real" 0)
				    '("require-k" "real" 0)
				    '("sns" "int" 0)
				    '("soil" "varchar" "medium")
				    '("size" "real" 0)
				    '("amount" "real" 0)
				    '("quality" "varchar" "DM2")
				    '("application" "varchar" "splash-surface")
				    '("season" "varchar" "winter")
				    '("crop" "varchar" "grass-grazed"))))

	 (vert-colour 
	  list-colour
	  (mtitle 'soil-info)
	  (mspinner 
	   'soil-type soil-type-list
	   (lambda (v) (entity-update-single-value! 
			(ktv "soil" "varchar" 
			     (spinner-choice soil-type-list v)))
		   (update-field-cropsoil-calc-from-current)))

	  (mspinner 
	   'regular-organic yesno-list 
	   (lambda (v) 
	     (entity-update-single-value! 
	      (ktv "regularly-manure" "varchar" 
		   (spinner-choice yesno-list v)))
	     (update-field-cropsoil-calc-from-current)))
	  
	  (mtext 'soil-test)
	  (horiz
	   (mspinner 
	    'soil-test-p soil-test-p-list 
	    (lambda (v) 
	      (entity-update-single-value! 
	       (ktv "soil-test-p" "varchar" 
		    (spinner-choice soil-test-p-list v)))
	      (update-field-cropsoil-calc-from-current)))
	   (mspinner 
	    'soil-test-k soil-test-k-list 
	    (lambda (v) 
	      (entity-update-single-value! 
	       (ktv "soil-test-k" "varchar" 
		    (spinner-choice soil-test-k-list v)))
	      (update-field-cropsoil-calc-from-current))))

	  (mtitle 'soil-supply)
	  (mtext-scale 'sns-output)
	  (text-view (make-id "supply-n") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
	  )

	 (vert-colour
	  list-colour
	  (mtitle 'crop-info)
	  (horiz
	   (mspinner 
	    'previous-crop-type previous-crop-type-list 
	    (lambda (v) 
	      (entity-update-single-value! 
	       (ktv "previous-crop" "varchar" 
		    (spinner-choice previous-crop-type-list v)))
	      (update-field-cropsoil-calc-from-current)))  
	   (mspinner 
	    'grown-grass yesno-list 
	    (lambda (v) 
	      (entity-update-single-value! 
	       (ktv "recently-grown-grass" "varchar" 
		    (spinner-choice yesno-list v)))'())))

	  (mspinner 'crop-type crop-type-list
		    (lambda (v) (entity-update-single-value! 
				 (ktv "crop" "varchar" 
				      (spinner-choice crop-type-list v))) 
			    (update-field-cropsoil-calc-from-current)))

	  (mtitle 'crop-requirements)
	  (horiz
	   (mtext-scale 'nutrient-n-output)
	   (mtext-scale 'nutrient-p-output)
	   (mtext-scale 'nutrient-k-output))
	  (horiz
	   (text-view (make-id "require-n") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
	   (text-view (make-id "require-p") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
	   (text-view (make-id "require-k") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0)))
	  )

	 (mtitle 'graph-title)
	 (canvas (make-id "graph")
		 (layout 'fill-parent 250 1 'centre 0)
		 (list))

	 (spacer 20)
	 
	 (horiz
	  (delete-button)
	  (mbutton-scale 'back (lambda () (list (finish-activity 99))))))))))))
   
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "farm" "field" (get-entity-by-unique db "farm" arg))
     (set-current! 'field-id arg)     
     (set-current! 'field-name (entity-get-value "name"))     
     (let ((polygons (get-polygons)))
       (let ((zoom (if (polygons-empty? polygons) zoom-out zoom-in)))
	 (append
	  (update-field-cropsoil-calc-from-current)
	  ;; if the current field is empty, keep the previous map position, don't recentre
	  (if (field-exists-yet? arg polygons) 
		(let ((centre (get-field-centre arg polygons)))
		  (msg "FIELD DATA CENTRING")		 
		  (list (update-widget 'draw-map (get-id "map") 'centre (list (vx centre) (vy centre) zoom))))
		(begin
		  (msg "NO FIELD DATA NOT CENTRING")
		  '()))
	  (list
	   (update-widget 'draw-map (get-id "map") 'polygons (list (entity-get-value "unique_id") (get-polygons)))
	   (mupdate 'edit-text 'field-name "name")
	   (update-list-widget db "farm" (list "date") "event" "eventview" (get-current 'field-id #f))
	   (mupdate-spinner 'soil-type "soil" soil-type-list)
	   (mupdate-spinner 'crop-type "crop" crop-type-list)
	   (update-widget 'edit-text (get-id "field-size") 'text (number->string (convert-output (entity-get-value "size") "hectares")))
	   (update-text-view-units 'field-size-text 'field-size 'field-size-i)
	   (update-widget 'canvas (get-id "graph") 'drawlist (build-graph))      
	   (mupdate-spinner 'previous-crop-type "previous-crop" previous-crop-type-list)
	   (mupdate-spinner 'soil-test-p "soil-test-p" soil-test-p-list)
	   (mupdate-spinner 'soil-test-k "soil-test-k" soil-test-k-list)
	   (mupdate-spinner 'regular-organic "regularly-manure" yesno-list)
	   (mupdate-spinner 'grown-grass "recently-grown-grass" yesno-list)
	   (update-text-view-units 'sns-output 'nutrient-n-metric 'nutrient-n-imperial)
	   (update-text-view-units 'nutrient-n-output 'nutrient-n-metric 'nutrient-n-imperial)
	   (update-text-view-units 'nutrient-p-output 'nutrient-p-metric 'nutrient-p-imperial)
	   (update-text-view-units 'nutrient-k-output 'nutrient-k-metric 'nutrient-k-imperial)
	   ;; updates for orientation change
	   (update-widget 'linear-layout (get-id "top") 'orientation (if (eq? screen-orientation 'portrait) 'vertical 'horizontal)) 
	   )))))
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

       (calc-manure-application-widget 
	(lambda (v)
	  (entity-set-value! "application" "varchar" (symbol->string v)))))
      
      (calc-amount-widget
       (lambda (v) 
	 (let ((amount (convert-input (* (current-seek-mul) v) (get-units))))
	   (entity-set-value! "amount" "real" amount))))
      
      (text-view (make-id "amount-value") "4500 gallons" 30
		 (layout 'wrap-content 'wrap-content 1 'centre 0))
      (spacer 10)
      (calc-event-results)

      (horiz
       (mbutton-scale 
	'save 
	(lambda () 
	  ;; update nutrient values now... (use available values)
	  (let* ((nutrients (calc-nutrients))
		 (total-nutrients (car nutrients))
		 (avail-nutrients (cadr nutrients)))
	    ;; store everything in metric
	    (if (eq? (list-ref total-nutrients 0) 'NA)
		(entity-set-value! "total-nutrients-n" "real" "0")
		(entity-set-value! "total-nutrients-n" "real" (list-ref total-nutrients 0)))
	    (entity-set-value! "total-nutrients-p" "real" (list-ref total-nutrients 1))
	    (entity-set-value! "total-nutrients-k" "real" (list-ref total-nutrients 2))	  
	    (if (eq? (list-ref avail-nutrients 0) 'NA)
		(entity-set-value! "nutrients-n" "real" "0")
		(entity-set-value! "nutrients-n" "real" (list-ref avail-nutrients 0)))
	    (entity-set-value! "nutrients-p" "real" (list-ref avail-nutrients 1))
	    (entity-set-value! "nutrients-k" "real" (list-ref avail-nutrients 2)))	  
	  (entity-update-values!) 
	  (list (finish-activity 0))))
       (mbutton-scale 
	'cancel 
	(lambda ()
	  (entity-set-value! "deleted" "int" 1)	  
	  (entity-update-values!) 
	  (list (finish-activity 0)))))
      )))
     
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'calc-mode 'fieldcalc)
     (entity-init! db "farm" "event" (get-entity-by-unique db "farm" arg))
     (set-current! 'event-id arg)
     ;; update the event and calculator with values from the current field
     (let ((field (get-entity-by-unique db "farm" (entity-get-value "parent"))))
       (update-crop! (string->symbol (ktv-get field "crop")))
       (update-soil! (string->symbol (ktv-get field "soil")))
       (update-fieldsize! (ktv-get field "size"))
       (update-soil-test! (list (string->symbol (ktv-get field "soil-test-p"))
				(string->symbol (ktv-get field "soil-test-k"))))
       ;; fill in the field related items here
       (entity-set-value! "crop" "varchar" (ktv-get field "crop"))
       (entity-set-value! "soil" "varchar" (ktv-get field "soil"))
       (entity-set-value! "size" "real" (ktv-get field "size"))
       (let ((results (get-crop-requirements/supply-from-field field)))
	 (entity-set-value! "require-n" "real" (convert-output (list-ref results 0) "kg/ha"))
	 (entity-set-value! "require-p" "real" (convert-output (list-ref results 1) "kg/ha"))
	 (entity-set-value! "require-k" "real" (convert-output (list-ref results 2) "kg/ha"))
	 (entity-set-value! "sns" "int" (list-ref results 3))       
	 (append
	  (update-field-cropsoil-calc results)
	  (list
	   (update-widget 'button (make-id "date") 'text (entity-get-value "date"))
	   (update-widget 'text-view (make-id "title") 'text (ktv-get field "name"))
	   (update-widget 'image-view (get-id "example") 'image
			  (find-image (string->symbol (entity-get-value "type"))
				      (entity-get-value "amount")))
	   (update-text-view-units 'nutrient-n-metric 'nutrient-n-metric 'nutrient-n-imperial)
	   (update-text-view-units 'nutrient-p-metric 'nutrient-p-metric 'nutrient-p-imperial)
 	   (update-text-view-units 'nutrient-k-metric 'nutrient-k-metric 'nutrient-k-imperial)
	   )))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))
  
  (activity
   "eventview"     
   (scroll-view-vert
    0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
    (list
     (vert
      (mtitle 'title)
      (event-view-item "type" 'report-type #t)
      (event-view-item "date" 'report-date #f)
      (event-view-item "amount" 'report-amount #t)
      (event-view-item "quality" 'report-quality #f)
      (event-view-item "application" 'report-application #t)
      (event-view-item "season" 'report-season #f)
      (event-view-item "crop" 'report-crop #t)
      (event-view-item "soil" 'report-soil #f)
      (event-view-item "size" 'report-size #t)
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

      )))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "farm" "event" (get-entity-by-unique db "farm" arg))
     (set-current! 'event-id arg)
     ;; update the calculator with values from the current field     
     (let ((field (get-entity-by-unique db "farm" (entity-get-value "parent")))
	   (total-amounts (list (entity-get-value "total-nutrients-n")
				(entity-get-value "total-nutrients-p")
				(entity-get-value "total-nutrients-k")))
	   (amounts (list (entity-get-value "nutrients-n")
			  (entity-get-value "nutrients-p")
			  (entity-get-value "nutrients-k"))))
       (list
	(update-widget 'text-view (make-id "title") 'text (ktv-get field "name"))
	(update-event-view-item-lookup "type")
	(update-event-view-item "date")
	(update-event-view-item "amount")
	(update-event-view-item-lookup "application")
	(update-event-view-item-lookup "quality")
	(update-event-view-item-lookup "season")
	(update-event-view-item-lookup "crop")
	(update-event-view-item-lookup "soil")
	(update-event-view-item "size")
	(update-widget 'text-view (get-id "na") 'text (string-append (number->string (convert-output (entity-get-value "nutrients-n") "kg/ha"))
								     " (" (dbg (number->string (convert-output (entity-get-value "total-nutrients-n") "kg/ha"))) ")"))
	(update-widget 'text-view (get-id "pa") 'text (string-append (number->string (convert-output (entity-get-value "nutrients-p") "kg/ha"))
								     " (" (number->string (convert-output (entity-get-value "total-nutrients-p") "kg/ha")) ")"))
	(update-widget 'text-view (get-id "ka") 'text (string-append (number->string (convert-output (entity-get-value "nutrients-k") "kg/ha"))
								     " (" (number->string (convert-output (entity-get-value "total-nutrients-k") "kg/ha")) ")"))
	(update-widget 'text-view (get-id "costn") 'text (get-cost-string-from-nutrient 0 amounts (entity-get-value "size")))
	(update-widget 'text-view (get-id "costp") 'text (get-cost-string-from-nutrient 1 amounts (entity-get-value "size")))
	(update-widget 'text-view (get-id "costk") 'text (get-cost-string-from-nutrient 2 amounts (entity-get-value "size")))
	(update-text-view-units 'size-text 'field-size 'field-size-i)
	(update-text-view-units 'nutrient-n-metric 'nutrient-n-metric 'nutrient-n-imperial)
	(update-text-view-units 'nutrient-p-metric 'nutrient-p-metric 'nutrient-p-imperial)
	(update-text-view-units 'nutrient-k-metric 'nutrient-k-metric 'nutrient-k-imperial)
	;; get tons/m3/gallons per ha/acre etc
	(update-widget 'text-view (get-id "amount-text") 'text 
		       (string-append (mtext-lookup 'report-amount) " (" 
				      (get-metric/imperial-units-for-type 
				       (string->symbol 
					(entity-get-value "type"))) ")"))
	)))
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
     (mbutton 'take-photo
	      (lambda ()
		(let ((path (photo-path)))
		  (list
		   (make-directory path)
		   (update-widget 'camera-preview (get-id "camera") 'take-picture path))))))
    (vert
     (image-view (make-id "example") "test" (layout 'fill-parent 320 1 'left 0))
     (mbutton 'back
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


  (activity
   "email"
   (vert
    (mtitle 'export)
    (mtext 'export-blurb)
    (mbutton 'email-button
	     (lambda ()
	       (save-data "fields.csv" (crap-csv db "farm" "event"))
	       (list
		(send-mail "" "From your Crap Calculator"
			   "Please find attached your field data."
			   (list (string-append dirname "fields.csv"))))))
    (mbutton 'factory-reset
	     (lambda ()
	       (list
		(alert-dialog
		 "factory-reset"
		 (mtext-lookup 'factory-reset-are-you-sure)
		 (lambda (v)
		   (when (eqv? v 1)
			 (nuke-database!)))))))
    (mbutton 'done (lambda () (list (finish-activity 99)))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  )
