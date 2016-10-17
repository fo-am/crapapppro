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
    (build-list-widget db "farm" 'fields (list "name") "field" "field"
 		       (lambda () #f)
		       (lambda ()
			 '(("name" "varchar" "None")
			   ("soil" "varchar" "None")
			   ("crop" "varchar" "None")
			   ("size" "real" 0))))

    (spacer 20)
    (mbutton 'calculator (lambda () (list (start-activity "calc" 2 ""))))
    (mbutton 'email (lambda () (list (start-activity "email" 2 ""))))
    (mbutton 'about (lambda () (list (start-activity "about" 2 ""))))
    (spacer 20)
    (mspinner 'choose-units units-list
	      (lambda (v) (mutate-units! (list-ref units-list v)) '())))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget 'draw-map (get-id "fieldmap") 'polygons (list "none highlighted" (get-polygons)))
      (update-list-widget db "farm" (list "name") "field" "field" #f)
      (update-widget 'spinner (get-id "choose-units-spinner") 'selection
                     (if (eq? (current-units) 'metric) 0 1))
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "calc"
   (vert
    (mtitle 'crap-calculator)
    (mspinner 
     'manure-type manure-type-list
     (lambda (v) 
       (let ((v (list-ref manure-type-list v)))
	 (update-seek-mul! v)
	 (append
	  (update-type! v)
	  (update-amount! (convert-input (* (current-seek-mul) 50) (get-units)))
	  (list
	   (update-widget 'seek-bar (get-id "amount") 'init 0)
	   (update-widget 'spinner (get-id "quality-spinner") 'array
			  (symbol-list-to-names
			   (get-qualities-for-type v)))	   
	   (update-widget 'image-view (get-id "example") 'image
			  (find-image (calc-type calc)
				      (calc-amount calc)))
	   )))))

    (horiz
     (mspinner 
      'soil-type soil-type-list 
      (lambda (v) (update-soil! (list-ref soil-type-list v))))

     (mspinner 
      'crop-type crop-type-list 
      (lambda (v) (update-crop! (list-ref crop-type-list v)))))

    (horiz
     (mspinner 'season season-list (lambda (v) (update-season! (list-ref season-list v))))
     (mspinner 'quality cattle-quality-list 
	       (lambda (v) 
		 (let ((type (current-type)))
		   (update-quality! 
		    (list-ref 
		     (cond
		      ((eq? type 'cattle) cattle-quality-list)
		      ((eq? type 'pig) pig-quality-list)
		      ((eq? type 'poultry) poultry-quality-list)
		      (else fym-quality-list))
		     v))))))
    
    (seek-bar (make-id "amount") 100 fillwrap
              (lambda (v)
		(msg (current-seek-mul)) 
                (append
                 (update-amount! (convert-input (* (current-seek-mul) v) (get-units)))
                 (list
                  (update-widget 'image-view (get-id "example") 'image
                                 (find-image (calc-type calc)
                                             (calc-amount calc)))))))
    
    (text-view (make-id "amount-value") "4500 gallons" 30
               (layout 'wrap-content 'wrap-content 1 'centre 0))
    (spacer 10)
    
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
    (spacer 10)
    (mbutton 'done (lambda () (list (finish-activity 99)))))
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
		     '()))
    (horiz
     (medit-text-scale 'field-name "normal" 
		       (lambda (v) (entity-update-single-value! 
				    (ktv "name" "varchar" v)) '()))
     (medit-text-scale 'field-size "numeric" 
		       (lambda (v)
			 (when (not (equal? v ""))
			       (entity-update-single-value! 
				(ktv "size" "real" (string->number v))) '()))))
    
    (horiz
     (mspinner 'soil-type soil-type-list
	       (lambda (v) (entity-update-single-value! 
			    (ktv "soil" "varchar" (spinner-choice soil-type-list v))) '())) 
     (mspinner 'crop-type crop-type-list
	       (lambda (v) (entity-update-single-value! 
			    (ktv "crop" "varchar" (spinner-choice crop-type-list v))) '())))
    
    
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
			       '("amount" "real" 0)
			       '("quality" "varchar" "DM2")
			       '("season" "varchar" "winter")
			       '("crop" "varchar" "normal"))))

    (spacer 20)
    
    (horiz
     (delete-button)
     (mbutton-scale 'back (lambda () (list (finish-activity 99))))))
   
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "farm" "field" (get-entity-by-unique db "farm" arg))
     (set-current! 'field-id arg)     
     (list
      (update-widget 'draw-map (get-id "map") 'polygons (list (entity-get-value "unique_id") (get-polygons)))
      (mupdate 'edit-text 'field-name "name")
      (update-list-widget db "farm" (list "type" "date") "event" "fieldcalc" (get-current 'field-id #f))
      (mupdate-spinner 'soil-type "soil" soil-type-list)
      (mupdate-spinner 'crop-type "crop" crop-type-list)
      (mupdate 'edit-text 'field-size "size")
					;(update-widget 'canvas (get-id "graph") 'drawlist (build-graph))      
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "fieldcalc"
   (vert
    (text-view (make-id "title") "field name" 40 fillwrap)
    (mtext 'field-calc-blurb) 
    (horiz
     (text-view 
      (make-id "date-text")
      (date->string (list date-day date-month date-year)) 25 wrap)
     (mbutton-scale 
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
		  (updatedb-current-nutrients 
		   (update-season! (date->season (current-date)))
		   (list
		    (update-widget 'text-view (make-id "date-text") 'text
				   (date->string (list day (+ month 1) year)))))))))))
     )
    
    (horiz
     (mspinner 
      'manure-type manure-type-list
      (lambda (v) 
	(let ((v (list-ref manure-type-list v)))
	  (entity-set-value! "type" "varchar" (symbol->string v))
	  (update-seek-mul! v)
	  (update-type! v)
	  (update-amount! (convert-input (* (current-seek-mul) 50) (get-units)))
	  (update-quality! (current-quality))
	  (append	   
	   (list
	    (update-widget 'spinner (get-id "quality-spinner") 'array (symbol-list-to-names (get-qualities-for-type v)))
	    
	    (update-widget 'seek-bar (get-id "amount") 'init 0)
	    (update-widget 'image-view (get-id "example") 'image
			   (find-image (calc-type calc)
				       (calc-amount calc)))
	    )))))
     
     (mspinner 'quality cattle-quality-list 
	       (lambda (v) 
		 (let ((type (current-type)))		   
		   (let ((quality
			  (list-ref 
			   (cond
			    ((eq? type 'cattle) cattle-quality-list)
			    ((eq? type 'pig) pig-quality-list)
			    ((eq? type 'poultry) poultry-quality-list)
			    (else fym-quality-list))
			   v)))
		     (entity-set-value! "quality" "varchar" (symbol->string quality))
		     (updatedb-current-nutrients 
		      (update-quality! quality)))))))
    
    (seek-bar (make-id "amount") 100 fillwrap
              (lambda (v)
		(let ((amount (convert-input (* (current-seek-mul) v) (get-units))))
		  (entity-set-value! "amount" "real" amount)
		  (append
		   (updatedb-current-nutrients 
		    (update-amount! amount))
		   (list
		    (update-widget 'image-view (get-id "example") 'image
				   (find-image (calc-type calc)
					       (calc-amount calc))))))))
	      
    (text-view (make-id "amount-value") "4500 gallons" 30
               (layout 'wrap-content 'wrap-content 1 'centre 0))
    (spacer 10)
    
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
    (spacer 10)

    (horiz
     (delete-button)
     (mbutton-scale 'save (lambda () (entity-update-values!) (list (finish-activity 0))))
     (mbutton-scale 'cancel (lambda () (list (finish-activity 0)))))
    )
    
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "farm" "event" (get-entity-by-unique db "farm" arg))
     (set-current! 'event-id arg)
     ;; update the calculator with values from the current field
     (let ((field (get-entity-by-unique db "farm" (entity-get-value "parent"))))
       (update-calc-from-db field)
       (list
	(mupdate-spinner 'quality "quality" (get-qualities-for-type (current-type)))
	(mupdate-spinner 'manure-type "type" manure-type-list)
	;;(update-widget 'seek-bar (get-id "amount") 'init (/ (convert-output (current-amount) (get-units)) (current-seek-mul)))
	(update-widget 'text-view (make-id "date-text") 'text (entity-get-value "date"))
	(update-widget 'text-view (make-id "title") 'text (ktv-get field "name"))
	)))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  )
