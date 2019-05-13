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
	       (lambda () 
		 (list (start-activity-goto 
			"main" 2 (get-setting-value "current-farm")))))
      (spacer 20)
      (mtext 'splash-discl)
      (image-view (make-id "about-logo") "logo" (layout 'wrap-content 'wrap-content 1 'centre 0))
      )))
   (lambda (activity arg)
     ;; check for farms in the db, add default one if none exist
     ;; so there is always one present...
     (when (null? (all-entities db "farm" "farm"))
	   (msg "no farms created yet, adding default")
	   (set-setting! 
	    "current-farm" "varchar"
	    (entity-init&save! 
	     db "farm" "farm"
	     (list '("name" "varchar" "My farm")
		   '("cost-n" "real" 0.79)
		   '("cost-p" "real" 0.62)
		   '("cost-k" "real" 0.49)
		   '("cost-m" "real" 0.49)
		   '("cost-s" "real" 0.49)
		   '("rainfall" "varchar" "rain-medium"))))     
	   ;; sent the parent of all the existing fields to be this farm
	   (for-each
	    (lambda (field-id)
	      (msg "updating to default farm" field-id)
	      (update-entity 
	       db "farm" field-id 
	       (list (ktv "parent" "varchar" (get-setting-value "current-farm")))))
	    (all-entities db "farm" "field")))
     
     (activity-layout activity))
   (lambda (activity arg) 
     ;; start gps for photos
     (list (gps-start "gps" (lambda (loc) (list)) 500 5)))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "farm"
   (linear-layout
     (make-id "top")
     'vertical
     (layout 'fill-parent 'wrap-content 1 'centre 0)
     '(0 0 0 0)
     (list 
      (scroll-view-vert
       0 (layout 'fill-parent 'wrap-content -1 'centre 0)
       (list
	(vert
	 (mtitle 'your-farms)
	 (mtext 'farm-info)  
	 (build-list-widget db "farm" 'farms-list (list "name") "farm" "main"
			    (lambda () #f)
			    (lambda ()
			      (list '("name" "varchar" "My farm")
				    '("cost-n" "real" 0.79)
				    '("cost-p" "real" 0.62)
				    '("cost-k" "real" 0.49)
				    '("cost-m" "real" 0.49)
				    '("cost-s" "real" 0.49)
				    '("rainfall" "varchar" "rain-medium"))))
	 
	 (spacer 20)
	 (mbutton-scale 'back (lambda () (list (finish-activity 99))))
					;(mbutton 'about (lambda () (list (start-activity "about" 2 ""))))
	 )))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-list-widget db "farm" (list "name") "farm" "main" #f)
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) 
     (msg requestcode resultcode)
     '()))


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
	  (mbutton 'farm-button (lambda () (list (start-activity "farm" 2 ""))))
	  (mbutton 'calculator (lambda () (list (start-activity "calc" 2 ""))))
	  (medit-text-scale 'farm-name "normal" 
			    (lambda (v) 
			      (entity-update-single-value! 
			       (ktv "name" "varchar" v)) 
			      ;; update the name on the map
			      (list)))

	  (build-list-widget db "farm" 'fields (list "name") "field" "field"
			     (lambda () (get-setting-value "current-farm"))
			     (lambda ()
			       (list '("name" "varchar" "My field")
				     (list "parent" "varchar" (get-setting-value "current-farm"))
				     '("soil" "varchar" "None")
				     (list "crop" "varchar" (params-list->text default-crop))
				     '("previous-crop" "varchar" "None")
				     '("soil-test-p" "varchar" "0")
				     '("soil-test-k" "varchar" "0")
				     '("soil-test-m" "varchar" "0")
				     '("regularly-manure" "varchar" "no")
				     '("recently-grown-grass" "varchar" "no")
				     '("size" "real" 0))))
	  
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
	   (medit-text-scale 'n-cost "numeric" (lambda (v) (entity-update-single-value! (ktv "cost-n" "real" v)) (update-costs) '()))
	   (medit-text-scale 'p-cost "numeric" (lambda (v) (entity-update-single-value! (ktv "cost-p" "real" v)) (update-costs) '()))
	   (medit-text-scale 'k-cost "numeric" (lambda (v) (entity-update-single-value! (ktv "cost-k" "real" v)) (update-costs) '()))
	   (medit-text-scale 'm-cost "numeric" (lambda (v) (entity-update-single-value! (ktv "cost-m" "real" v)) (update-costs) '()))
	   (medit-text-scale 's-cost "numeric" (lambda (v) (entity-update-single-value! (ktv "cost-s" "real" v)) (update-costs) '()))
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
				  ("K" "real" 0)
				  ("S" "real" 0)
				  ("M" "real" 0)
				  ("type" "varchar" ""))))
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
			   (update-widget 'text-view (get-id "k-cost-text") 'text (mtext-lookup 'k-cost))
			   (update-widget 'text-view (get-id "m-cost-text") 'text (mtext-lookup 'm-cost))
			   (update-widget 'text-view (get-id "s-cost-text") 'text (mtext-lookup 's-cost)))
			  (list
			   (update-widget 'text-view (get-id "n-cost-text") 'text (mtext-lookup 'n-cost-imperial))
			   (update-widget 'text-view (get-id "p-cost-text") 'text (mtext-lookup 'p-cost-imperial))
			   (update-widget 'text-view (get-id "k-cost-text") 'text (mtext-lookup 'k-cost-imperial))
			   (update-widget 'text-view (get-id "m-cost-text") 'text (mtext-lookup 'm-cost-imperial))
			   (update-widget 'text-view (get-id "s-cost-text") 'text (mtext-lookup 's-cost-imperial))))))
	  (mspinner 'rainfall rainfall-list
		    (lambda (v) 
		      (entity-update-single-value! 
		       (ktv "rainfall" "varchar" 
			    (symbol->string (list-ref rainfall-list v)))) 
		      (update-rainfall) 
		      '())))
	 (mbutton 'email (lambda () (list (start-activity "email" 2 ""))))
	 ;;(mbutton 'about (lambda () (list (start-activity "about" 2 ""))))
	 ))))))
   
   
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "farm" "farm" (get-entity-by-unique db "farm" arg))
     (set-setting! "current-farm" "varchar" arg)
     ;; pull the costs and rainfall in for this farm
     (update-costs)
     (update-rainfall)
     (let ((polygons (get-polygons)))
       (let ((zoom (if (polygons-empty? polygons) zoom-out zoom-in))
	     (centre (get-farm-centre polygons)))
	 (append
	  (check-backup)
	  (if (equal? (current-units) 'metric) 
	      '()
	      (list
	       (update-widget 'text-view (get-id "n-cost-text") 'text (mtext-lookup 'n-cost-imperial))
	       (update-widget 'text-view (get-id "p-cost-text") 'text (mtext-lookup 'p-cost-imperial))
	       (update-widget 'text-view (get-id "k-cost-text") 'text (mtext-lookup 'k-cost-imperial))
	       (update-widget 'text-view (get-id "m-cost-text") 'text (mtext-lookup 'm-cost-imperial))
	       (update-widget 'text-view (get-id "s-cost-text") 'text (mtext-lookup 's-cost-imperial))))
	  (list	  
	   (mupdate 'edit-text 'farm-name "name")
	   (update-widget 'toggle-button (get-id "custom-manures") 'checked 0) 
	   (update-widget 'linear-layout (get-id "costs-list") 'hide 1)
	   (update-widget 'linear-layout (get-id "custom-manures-list") 'hide 1)
	   (update-widget 'edit-text (get-id "n-cost") 'text (number->string (rounding-cash (entity-get-value "cost-n"))))
	   (update-widget 'edit-text (get-id "p-cost") 'text (number->string (rounding-cash (entity-get-value "cost-p"))))
	   (update-widget 'edit-text (get-id "k-cost") 'text (number->string (rounding-cash (entity-get-value "cost-k"))))
	   (update-widget 'edit-text (get-id "m-cost") 'text (number->string (rounding-cash (entity-get-value "cost-m"))))
	   (update-widget 'edit-text (get-id "s-cost") 'text (number->string (rounding-cash (entity-get-value "cost-s"))))
	   (update-widget 'draw-map (get-id "fieldmap") 'polygons (list "none highlighted" (get-polygons)))
	   (update-widget 'draw-map (get-id "fieldmap") 'centre (list (vx centre) (vy centre) zoom))
	   (update-list-widget db "farm" (list "name") "field" "field" (get-setting-value "current-farm"))
	   (update-list-widget db "farm" (list "name") "manure" "manure" #f)
	   (update-widget 'spinner (get-id "choose-units-spinner") 'selection
			  (if (eq? (current-units) 'metric) 0 1))
	   (update-widget 'spinner (get-id "rainfall-spinner") 'selection
			  (index-find (string->symbol (entity-get-value "rainfall")) rainfall-list))
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
	 'crop-type crop-type-for-manure-calc-list 
	 (lambda (v)
	   (update-crop! 
	    ;; need to convert this simple choice into some crop parameters
	    (crop-type-for-manure->crop-params 
	     (list-ref crop-type-for-manure-calc-list v))))))
       
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
     (list
      (update-text-view-units 'crop-available 'crop-available-metric 'crop-available-imperial)))
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
				       (convert-output->string (entity-get-value "size") 
							       "hectares")))))
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
				    '("nutrients-s" "real" 0)
				    '("nutrients-m" "real" 0)
				    '("total-nutrients-n" "real" 0)
				    '("total-nutrients-p" "real" 0)
				    '("total-nutrients-k" "real" 0)
				    '("total-nutrients-s" "real" 0)
				    '("total-nutrients-m" "real" 0)
				    '("require-n" "real" 0)
				    '("require-p" "real" 0)
				    '("require-k" "real" 0)
				    '("require-s" "real" 0)
				    '("require-m" "real" 0)
				    '("sns" "int" 0)
				    '("soil" "varchar" "medium")
				    '("size" "real" 0)
				    '("amount" "real" 0)
				    '("quality" "varchar" "DM2")
				    '("application" "varchar" "splash-surface")
				    '("season" "varchar" "winter")
				    (list "crop" "varchar" (params-list->text default-crop)))))
	 
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
	      (update-field-cropsoil-calc-from-current)))
	   (mspinner 
	    'soil-test-m soil-test-m-list 
	    (lambda (v) 
	      (entity-update-single-value! 
	       (ktv "soil-test-m" "varchar" 
		    (spinner-choice soil-test-m-list v)))
	      (update-field-cropsoil-calc-from-current))))

	  (mtitle 'soil-supply)
	  (text-view (make-id "supply-n") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
	  (mtitle 'sulphur-risk)
	  (mtext 'sulphur-risk-expl)
	  (text-view (make-id "risk-s") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
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
	      ;; if we have grown grass as previous crop
	      (cond
	       ((previous-crop-grass? (list-ref previous-crop-type-list v))
		;; automatically select grown-grass
		;; dont' automatically deselect it, as it could 
		;; have been set for the previous 3 years
		(entity-update-single-value! (ktv "recently-grown-grass" "varchar" "yes"))
		(append
		 (list (mupdate-spinner 'grown-grass "recently-grown-grass" yesno-list))
		 (update-field-cropsoil-calc-from-current)))
	       (else
		;; otherwise leave it as it is
		(update-field-cropsoil-calc-from-current)))))
	   (mspinner 
	    'grown-grass yesno-list 
	    (lambda (v) 
	      (entity-update-single-value! 
	       (ktv "recently-grown-grass" "varchar" 
		    (spinner-choice yesno-list v)))'())))

	  (text-view (make-id "crop-output") "Current crop" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))

	  (linear-layout
	   (make-id "crop-details") 'vertical
	   (layout 'fill-parent 'wrap-content 1 'centre margin-size)
	   (list 0 0 0 0)
	   '())

	  (mbutton 'crop-type
		   (lambda ()
		     (set-current! 'crop-menu-options '())
		     (list (start-activity "cropselect" 0 ""))))
	  
	  (mtitle 'crop-requirements-ind)
	  (horiz
	   (mtext-scale 'nutrient-n)
	   (mtext-scale 'nutrient-p)
	   (mtext-scale 'nutrient-k)
	   (mtext-scale 'nutrient-s)
	   (mtext-scale 'nutrient-m))
	  (horiz
	   (text-view (make-id "require-n") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
	   (text-view (make-id "require-p") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
	   (text-view (make-id "require-k") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
	   (text-view (make-id "require-s") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
	   (text-view (make-id "require-m") "0" 30 (layout 'wrap-content 'wrap-content 1 'centre 0)))
	  )

	 (mtitle 'graph-title)
	 (canvas (make-id "graph")
		 (layout 'fill-parent 250 1 'centre 0)
		 (list))

	 (spacer 20)
	 
	 (horiz
	  (delete-button)
	  (mbutton-scale 'field-back (lambda () (list (finish-activity 99))))))))))))
   
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
		(list (update-widget 'draw-map (get-id "map") 'centre (list (vx centre) (vy centre) zoom))))
	      (begin
		'()))
	  (update-crop-details-from-current)
	  (list
	   (update-widget 'draw-map (get-id "map") 'polygons (list (entity-get-value "unique_id") (get-polygons)))
	   (mupdate 'edit-text 'field-name "name")
	   (update-list-widget db "farm" (list "date") "event" "eventview" (get-current 'field-id #f))
	   (mupdate-spinner 'soil-type "soil" soil-type-list)
	   (update-widget 'edit-text (get-id "field-size") 'text (convert-output->string (entity-get-value "size") "hectares"))
	   (update-text-view-units 'field-size-text 'field-size 'field-size-i)
	   (update-widget 'canvas (get-id "graph") 'drawlist (build-graph))      
	   (mupdate-spinner 'previous-crop-type "previous-crop" previous-crop-type-list)
	   (mupdate-spinner 'soil-test-p "soil-test-p" soil-test-p-list)
	   (mupdate-spinner 'soil-test-k "soil-test-k" soil-test-k-list)
	   (mupdate-spinner 'soil-test-m "soil-test-m" soil-test-m-list)
	   (mupdate-spinner 'regular-organic "regularly-manure" yesno-list)
	   (mupdate-spinner 'grown-grass "recently-grown-grass" yesno-list)
	   (update-text-view-units 'crop-requirements-ind 'crop-requirements-metric 'crop-requirements-imperial)
	   (update-text-view-units 'sns-output 'nutrient-n-metric 'nutrient-n-imperial)
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
	    (if (eq? (list-ref total-nutrients 1) 'NA)
		(entity-set-value! "total-nutrients-p" "real" "0")
		(entity-set-value! "total-nutrients-p" "real" (list-ref total-nutrients 1)))
	    (if (eq? (list-ref total-nutrients 2) 'NA)
		(entity-set-value! "total-nutrients-k" "real" "0")	  
		(entity-set-value! "total-nutrients-k" "real" (list-ref total-nutrients 2)))	  
	    (if (eq? (list-ref total-nutrients 3) 'NA)
		(entity-set-value! "total-nutrients-s" "real" "0")	  
		(entity-set-value! "total-nutrients-s" "real" (list-ref total-nutrients 3)))	  
	    (if (eq? (list-ref total-nutrients 2) 'NA)
		(entity-set-value! "total-nutrients-m" "real" "0")	  
		(entity-set-value! "total-nutrients-m" "real" (list-ref total-nutrients 4)))	  
	    
	    (if (eq? (list-ref avail-nutrients 0) 'NA)
		(entity-set-value! "nutrients-n" "real" "0")
		(entity-set-value! "nutrients-n" "real" (list-ref avail-nutrients 0)))
	    (if (eq? (list-ref avail-nutrients 1) 'NA)
		(entity-set-value! "nutrients-p" "real" "0")
		(entity-set-value! "nutrients-p" "real" (list-ref avail-nutrients 1)))
	    (if (eq? (list-ref avail-nutrients 2) 'NA)
		(entity-set-value! "nutrients-k" "real" "0")	  
		(entity-set-value! "nutrients-k" "real" (list-ref avail-nutrients 2)))
	    (if (eq? (list-ref avail-nutrients 3) 'NA)
		(entity-set-value! "nutrients-s" "real" "0")	  
		(entity-set-value! "nutrients-s" "real" (list-ref avail-nutrients 3)))
	    (if (eq? (list-ref avail-nutrients 4) 'NA)
		(entity-set-value! "nutrients-m" "real" "0")	  
		(entity-set-value! "nutrients-m" "real" (list-ref avail-nutrients 4))))
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
       (update-crop! (text->params-list (ktv-get field "crop")))
       (update-soil! (string->symbol (ktv-get field "soil")))
       (update-fieldsize! (ktv-get field "size"))
       (update-soil-test! (list (string->symbol (ktv-get field "soil-test-p"))
				(string->symbol (ktv-get field "soil-test-k"))
				(string->symbol (ktv-get field "soil-test-m"))))
       ;; fill in the field related items here
       (entity-set-value! "crop" "varchar" (ktv-get field "crop"))
       (entity-set-value! "soil" "varchar" (ktv-get field "soil"))
       (entity-set-value! "size" "real" (ktv-get field "size"))
       (let ((results (get-crop-requirements/supply-from-field field (symbol->string (date->month (current-date))))))
	 (entity-set-value! "require-n" "real" (convert-output (list-ref results 0) "kg/ha"))
	 (entity-set-value! "require-p" "real" (convert-output (list-ref results 1) "kg/ha"))
	 (entity-set-value! "require-k" "real" (convert-output (list-ref results 2) "kg/ha"))
	 (entity-set-value! "require-s" "real" (convert-output (list-ref results 3) "kg/ha"))
	 (entity-set-value! "require-m" "real" (convert-output (list-ref results 4) "kg/ha"))
	 (entity-set-value! "sns" "int" (list-ref results 5))       
	 (append
	  (update-field-cropsoil-calc results)
	  (list
	   (update-widget 'button (make-id "date") 'text (entity-get-value "date"))
	   (update-widget 'text-view (make-id "title") 'text (ktv-get field "name"))
	   (update-widget 'image-view (get-id "example") 'image
			  (find-image (string->symbol (entity-get-value "type"))
				      (string->symbol (entity-get-value "quality"))
				      (entity-get-value "amount")))
	   (update-text-view-units 'crop-available 'crop-available-metric 'crop-available-imperial)))
	 )))
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
      (horiz
       (delete-button)
       (mbutton-scale 
	'camera-button
	(lambda ()
	  (list (start-activity "camera" 2 "")))))
      (spacer 10)
      (calc-gallery)
      (spacer 10)
      (mbutton-scale 'back (lambda () (list (finish-activity 0))))
      
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
				(entity-get-value "total-nutrients-k")
				(entity-get-value "total-nutrients-s")
				(entity-get-value "total-nutrients-m")))
	   (amounts (list (entity-get-value "nutrients-n")
			  (entity-get-value "nutrients-p")
			  (entity-get-value "nutrients-k")
			  (entity-get-value "nutrients-s")
			  (entity-get-value "nutrients-m"))))
       (list
	(update-widget 'text-view (make-id "title") 'text (ktv-get field "name"))
	(update-event-view-item-lookup "type")
	(update-event-view-item "date")
	(update-event-view-item "amount")
	(update-event-view-item-lookup "application")
	(update-event-view-item-lookup "quality")
	(update-event-view-item-lookup "season")
	(update-event-view-item "crop")
	(update-event-view-item-lookup "soil")
	(update-event-view-item "size")
	(update-widget 'text-view (get-id "na") 'text (convert-output->string (entity-get-value "nutrients-n") "kg/ha"))
	(update-widget 'text-view (get-id "pa") 'text (convert-output->string (entity-get-value "nutrients-p") "kg/ha"))								     
	(update-widget 'text-view (get-id "ka") 'text (convert-output->string (entity-get-value "nutrients-k") "kg/ha"))
	(update-widget 'text-view (get-id "sa") 'text (convert-output->string (entity-get-value "nutrients-s") "kg/ha"))
	(update-widget 'text-view (get-id "ma") 'text (convert-output->string (entity-get-value "nutrients-m") "kg/ha"))
	(update-widget 'text-view (get-id "nt") 'text (convert-output->string (entity-get-value "total-nutrients-n") "kg/ha"))
	(update-widget 'text-view (get-id "pt") 'text (convert-output->string (entity-get-value "total-nutrients-p") "kg/ha"))
	(update-widget 'text-view (get-id "kt") 'text (convert-output->string (entity-get-value "total-nutrients-k") "kg/ha"))
	(update-widget 'text-view (get-id "st") 'text (convert-output->string (entity-get-value "total-nutrients-s") "kg/ha"))
	(update-widget 'text-view (get-id "mt") 'text (convert-output->string (entity-get-value "total-nutrients-m") "kg/ha"))
	(update-widget 'text-view (get-id "costn") 'text (get-cost-string-from-nutrient 0 amounts (entity-get-value "size")))
	(update-widget 'text-view (get-id "costp") 'text (get-cost-string-from-nutrient 1 amounts (entity-get-value "size")))
	(update-widget 'text-view (get-id "costk") 'text (get-cost-string-from-nutrient 2 amounts (entity-get-value "size")))
	(update-widget 'text-view (get-id "costs") 'text (get-cost-string-from-nutrient 3 amounts (entity-get-value "size")))
	(update-widget 'text-view (get-id "costm") 'text (get-cost-string-from-nutrient 4 amounts (entity-get-value "size")))
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
                                 (string->symbol (entity-get-value "quality"))
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
      (medit-text 'manure-s "numeric" (lambda (v) (entity-update-single-value! (ktv "S" "real" (safe-string->number v))) '()))
      (medit-text 'manure-m "numeric" (lambda (v) (entity-update-single-value! (ktv "M" "real" (safe-string->number v))) '()))
      (mspinner 'custom-manure-type custom-manure-type-list
		(lambda (v) 
		  (entity-update-single-value! (ktv "type" "varchar" (list-ref custom-manure-type-list v))) '()))
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
      (mupdate 'edit-text 'manure-s "S")
      (mupdate 'edit-text 'manure-m "M")
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "email"
   (scroll-view-vert
    0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
    (list
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
      (spacer 20)

      (mtitle 'send-farm-title)
      (mtext 'send-farm-blurb)
      (horiz
       (medit-text-scale 'password "password" 
			 (lambda (v)
			   ;; store in memory only
			   (set-current! 'password v)
			   ;; update the name on the map
			   (list)))
       (mtoggle-button-scale 
	'view-password 
	(lambda (v) 
	  (list 
	   (update-widget 
	    'edit-text (get-id "password") 
	    'input-type (if (zero? v) "password" "visible-password"))))))
      
      (mbutton 'email-farm-button
	       (lambda ()
		 (save-data "farm.crap.json" (export-current-farm-as-json))
		 (list
		  (send-mail-encrypt
		   "" "Farm data"
		   "You have been sent farm data from the Farm Crap App. To import this data into your app, click on the attachment to launch the Farm Crap App importer."
		   (list (string-append dirname "farm.crap.json"))
		   (get-current 'password "crapapp")))))
      
      (mspinner 'backup-freq backup-freq-list
		(lambda (v) 
		  (set-setting! "backup-freq" "varchar" (symbol->string (list-ref backup-freq-list v)))))
      (mtext 'backup-blurb)

      (spacer 20)    
      (mtitle 'reset-title)
      (mbutton 'factory-reset
	       (lambda ()
		 (list
		  (alert-dialog
		   "factory-reset"
		   (mtext-lookup 'factory-reset-are-you-sure)
		   (lambda (v)
		     (when (eqv? v 1)
			   (nuke-database!)))))))
      (mbutton 'done (lambda () (list (finish-activity 99)))))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) 
     (list 
      (update-widget 'edit-text (get-id "password") 'text (get-current 'password "crapapp"))
      (update-widget 'spinner (get-id "backup-freq-spinner") 'selection
		     (cond
		      ((not (get-setting-value "backup-freq")) 0)
		      ((equal? (get-setting-value "backup-freq") "never") 0)
		      ((equal? (get-setting-value "backup-freq") "daily") 1)
		      ((equal? (get-setting-value "backup-freq") "weekly") 2)
		      (else 3)))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "backup"
   (vert
    (mtitle 'timed-backup)
    (mtext 'timed-backup-blurb)
    (horiz
     (medit-text-scale 'password "password" 
		       (lambda (v)
			 ;; store in memory only
			 (set-current! 'password v)
			 ;; update the name on the map
			 (list)))
     (mtoggle-button-scale 
      'view-password 
      (lambda (v) 
	(list 
	 (update-widget 
	  'edit-text (get-id "password") 
	  'input-type (if (zero? v) "password" "visible-password"))))))
    
    (mbutton 'email-farm-button
	     (lambda ()
	       (list
		(encrypt
		 "export-encrypt"
		 (export-current-farm-as-json)
		 (get-current 'password "crapapp")
		 (lambda (ciphertext)
		   (set-setting! "last-backup" "varchar" (date->string (current-date)))
		   (save-data "farm.crap.json.enc" ciphertext)
		   (list
		    (send-mail "" "From your Crap Calculator"
			       "Please find attached your farm data."
			       (list (string-append dirname "farm.crap.json.enc")))))))))
    
    (mbutton 'done (lambda () (list (finish-activity 99)))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) 
     (list 
      (update-widget 'edit-text (get-id "password") 'text (get-current 'password "crapapp"))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "cropselect"
   (scroll-view-vert
    0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
    (list
   (vert
    (mtitle 'crop-select)
    
    ;; to store the current selection list
    (linear-layout
     (make-id "crop-select-list") 'horizontal
     (layout 'fill-parent 'wrap-content 1 'centre margin-size)
     (list 0 0 0 0)
     '())

    (text-view (make-id "crop-select-category") "" 30
	       (layout 'wrap-content 'wrap-content 1 'centre 0))
    
    ;; current selection buttons
    (scroll-view-vert
     0 (layout 'fill-parent 'wrap-content 1 'centre 0)
     (list
      (linear-layout
       (make-id "crop-select-buttons") 'vertical
       (layout 'fill-parent 'wrap-content 1 'centre margin-size)
       (list 0 0 0 0)
       '())))
     
    (horiz
     (mbutton-scale 
      'back (lambda () 
	      (set-current! 
	       'crop-menu-options 
	       ;; chop off the last (cumbersomely)
	       (reverse (cdr (reverse (get-current 'crop-menu-options '())))))
	      (list
	       ;; for now just clear the selection indicator
	       (rebuild-crop-select-list (get-current 'crop-menu-options '()))
	       (update-tree-menu 
		"crop-select-buttons"
		crop-tree-menu (get-current 'crop-menu-options '())))))
     (mbutton-scale 'cancel (lambda () (list (finish-activity 99))))))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) 
     (list
      (rebuild-crop-select-list (get-current 'crop-menu-options '()))
      (update-tree-menu 
       "crop-select-buttons"
       crop-tree-menu (get-current 'crop-menu-options '()))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "import"
   (scroll-view-vert
    0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
    (list
     (vert
      (mtitle 'import-farm)
      (mtext 'import-blurb)
      (spacer 20)
      (horiz
       (medit-text-scale 'password "password" 
			 (lambda (v)
			   ;; store in memory only
			   (set-current! 'password v)
			   ;; update the name on the map
			   (list)))
       (mtoggle-button-scale 
	'view-password 
	(lambda (v) 
	  (list 
	   (update-widget 
	    'edit-text (get-id "password") 
	    'input-type (if (zero? v) "password" "visible-password"))))))
      
      (mbutton 'import-farm 
	       (lambda () 
		 (list
		  (decrypt 
		   "import-decrypt"
		   (string-append dirname "farm.crap.json.enc")
		   (get-current 'password "crapapp")
		   (string-append dirname "farm.crap.json")
		   (lambda (success)
		     (msg success)
		     (cond 
		      ((not success)
		       (list
			(ok-dialog
			 "bad-password"
			 (mtext-lookup 'bad-password)
			 (lambda ()))))
		      (else
		       (let* ((import-data (json/parse-file (string-append dirname "farm.crap.json")))
			      (farm-name (farm-name import-data)))
			 (list
			  (alert-dialog	
			   "import-farm"
			   (if (farm-exists? db "farm" import-data)
			       (string-append (mtext-lookup 'import-existing-farm) farm-name)
			       (string-append (mtext-lookup 'import-new-farm) farm-name))
			   (lambda (v)
			     (if (eqv? v 1)
				 (let ((import-result (import-farm db "farm" import-data)))
				   (list
				    (if import-result
					(build-import-report import-result farm-name)
					(ok-dialog
					 "bad-file-version"
					 (mtext-lookup 'bad-file-version)
					 (lambda ())))))
				 '()))))))))))))
      
      (linear-layout
       (make-id "import-report")
       'vertical
       (layout 'fill-parent 'wrap-content 1 'centre 0)
       '(0 0 0 0)
       (list))
      
      (horiz
       (mbutton-scale 'return-to-app (lambda () (list (start-activity-goto "splash" 0 ""))))))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list 
      (update-widget 'edit-text (get-id "password") 'text (get-current 'password "crapapp"))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))
  

  
  )
