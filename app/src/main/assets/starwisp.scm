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
