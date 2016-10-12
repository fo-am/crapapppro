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
    (build-drawmap-fragment (make-id "map") fillwrap)
    (build-list-widget db "farm" 'fields (list "name") "field" "field"
		       (lambda () #f)
		       (lambda ()
			 '(("name" "varchar" "None")
			   ("soil" "varchar" "None")
			   ("crop" "varchar" "None")
			   ("size" "real" 0))))

    (spacer 20)
    (mspinner 'choose-units units-list
	      (lambda (v)
		;;(mutate-units! (list-ref (list metric imperial) v))
               (list)))
    (spacer 20)
    (mbutton 'calculator (lambda () (list (start-activity "calc" 2 ""))))
    (mbutton 'email (lambda () (list (start-activity "email" 2 ""))))
    (mbutton 'about (lambda () (list (start-activity "about" 2 "")))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-list-widget db "farm" (list "name") "field" "field" #f)
      ;(update-widget 'spinner (get-id "units") 'selection
      ;               (if (equal? (current-units) metric) 0 1))
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
	  (update-type! "c" v)
	  (update-amount! "c" (convert-input (* (current-seek-mul) 50) (get-units)))
	  (list
	   (update-widget 'seek-bar (get-id "amount") 'init 0)
	   (update-widget 'spinner (get-id "quality-spinner") 'array
			  (dbg (symbol-list-to-names
				(cond
				 ((eq? v 'cattle) cattle-quality-list)
				 ((eq? v 'pig) pig-quality-list)
				 ((eq? v 'poultry) poultry-quality-list)
				 (else fym-quality-list)))))
	   
	   (update-widget 'image-view (get-id "example") 'image
			  (find-image (calc-type (current-calc))
				      (calc-amount (current-calc))))
	   )))))

    (horiz
     (mspinner 
      'soil-type soil-type-list 
      (lambda (v) (update-soil! "c" (list-ref soil-type-list v))))

     (mspinner 
      'crop-type crop-type-list 
      (lambda (v) (update-crop! "c" (list-ref crop-type-list v)))))

    (horiz
     (mspinner 'season season-list (lambda (v) (update-season! "c" (list-ref season-list v))))
     (mspinner 'quality cattle-quality-list (lambda (v) '())))
    
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
    
    (mtext 'crop-availible)
    (horiz
     (mtext-scale 'nutrient-n-metric)
     (mtext-scale 'nutrient-p-metric)
     (mtext-scale 'nutrient-k-metric))
    (horiz
     (text-view (make-id "cna") "12" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
     (text-view (make-id "cpa") "12" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
     (text-view (make-id "cka") "12" 30 (layout 'wrap-content 'wrap-content 1 'centre 0)))
    
    (mtext 'cost-saving)
    (horiz
     (text-view (make-id "ccostn") "12" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
     (text-view (make-id "ccostp") "12" 30 (layout 'wrap-content 'wrap-content 1 'centre 0))
     (text-view (make-id "ccostk") "12" 30 (layout 'wrap-content 'wrap-content 1 'centre 0)))
    (spacer 10)
    (image-view (make-id "example") "test" (layout 'fill-parent 'fill-parent 1 'centre 0))
    (spacer 10)
    (mbutton 'done (lambda () '())))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  
  (activity
   "field"
   (vert
    (medit-text 'field-name "normal" 
		(lambda (v) (entity-update-single-value! 
			     (ktv "name" "varchar" v)) '()))

    (mspinner 'soil-type soil-type-list
	      (lambda (v) (entity-update-single-value! 
			   (ktv "soil" "varchar" (spinner-choice soil-type-list v))) '()))

    (mspinner 'crop-type crop-type-list
	      (lambda (v) (entity-update-single-value! 
			   (ktv "crop" "varchar" (spinner-choice crop-type-list v))) '()))
    
    (medit-text 'field-size "numeric" 
		(lambda (v) (entity-update-single-value! (ktv "size" "real" (string->number v))) '()))
    
    (canvas (make-id "graph")
	    (layout 'fill-parent 250 1 'centre 0)
	    (list))

    (build-list-widget db "farm" 'events (list "date" "type") "event" "fieldcalc"
		       (lambda () #f)
		       (lambda ()
			 '(("id-field" "int" 0)
			   ("type" "varchar" "None")
			   ("date" "varchar" "None")
			   ("nutrients-n" "real" 0)
			   ("nutrients-p" "real" 0)
			   ("nutrients-k" "real" 0)
			   ("amount" "real" 0)
			   ("quality" "varchar" "None")
			   ("season" "varchar" "None")
			   ("crop" "varchar" "None"))))

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
      (mupdate 'edit-text 'field-name "name")
      (update-list-widget db "farm" (list "date" "type") "event" "event" #f)
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


  )
