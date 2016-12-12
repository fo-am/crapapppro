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

(define i18n-lang 0)

(define i18n-text
  (list))

(define large-text-size 30)
(define button-text-size 30)
(define normal-text-size 20)
(define small-text-size 15)
(define margin-size 10)
(define list-colour (list 127 255 127 50))

(define (mtext-lookup id)
  (define (_ l)
    (cond
     ((null? l) (string-append (symbol->string id) " not translated"))
     ((eq? (car (car l)) id)
      (let ((translations (cadr (car l))))
        (if (<= (length translations) i18n-lang)
            (string-append (symbol->string id) " not translated")
            (let ((r (list-ref translations i18n-lang)))
              (if (or (equal? r "") (equal? r " "))
                  (list-ref translations 0) r)))))
     (else (_ (cdr l)))))
  (_ i18n-text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (symbol->id id)
  (when (not (symbol? id))
        (msg "symbol->id: [" id "] is not a symbol"))
  (make-id (symbol->string id)))

(define (get-symbol-id id)
  (when (not (symbol? id))
        (msg "symbol->id: [" id "] is not a symbol"))
  (get-id (symbol->string id)))


(define (spacer size) (space (layout 'fill-parent size 1 'left 0)))


(define (horiz . l)
  (linear-layout
   0 'horizontal
   (layout 'fill-parent 'wrap-content -1 'centre 0)
   (list 0 0 0 0)
   l))

(define (horiz-colour col . l)
  (linear-layout
   0 'horizontal
   (layout 'fill-parent 'wrap-content -1 'centre 0)
   col
   l))

(define (vert . l)
  (linear-layout
   0 'vertical
   (layout 'fill-parent 'wrap-content 1 'centre margin-size)
   (list 0 0 0 0)
   l))

(define (vert-colour col . l)
  (linear-layout
   0 'vertical
   (layout 'fill-parent 'wrap-content 1 'centre margin-size)
   col
   l))

(define (vert-fill . l)
  (linear-layout
   0 'vertical
   (layout 'fill-parent 'fill-parent 1 'left 0)
   (list 0 0 0 0)
   l))

(define (relative rules colour . l)
  (relative-layout
   0 (rlayout 'fill-parent 'wrap-content (list 20 20 20 20) rules)
   colour
   l))

(define (mbutton id fn)
  (button (symbol->id id)
          (mtext-lookup id)
          button-text-size (layout 'fill-parent 'wrap-content -1 'centre 5) fn))

(define (mbutton-scale id fn)
  (button (symbol->id id)
          (mtext-lookup id)
          button-text-size
	  (layout 'fill-parent 'wrap-content 1 'centre 5) fn))

(define (mtoggle-button id fn)
  (toggle-button (symbol->id id)
                 (mtext-lookup id)
                 button-text-size 
		 (layout 'fill-parent 'wrap-content -1 'centre 5) ""
                 ;; convert to 0/1 for easier db storage
                 (lambda (v) (fn (if v 1 0)))))

(define (mtoggle-button-scale id fn)
  (toggle-button (symbol->id id)
                 (mtext-lookup id)
                 button-text-size
		 (layout 'fill-parent 'wrap-content 1 'centre 5) ""
                 (lambda (v) (fn (if v 1 0)))))

(define (mtext id)
  (text-view (symbol->id id)
             (mtext-lookup id)
             normal-text-size (layout 'wrap-content 'wrap-content -1 'centre 0)))

(define (mtext-fixed w id)
  (text-view (symbol->id id)
             (mtext-lookup id)
             normal-text-size (layout w 'wrap-content -1 'centre 0)))

(define (mtext-small id)
  (text-view (symbol->id id)
             (mtext-lookup id)
             small-text-size (layout 'wrap-content 'wrap-content -1 'centre 0)))

(define (mtext-scale id)
  (text-view (symbol->id id)
             (mtext-lookup id)
             normal-text-size (layout 'wrap-content 'wrap-content 1 'centre 0)))

(define (mtitle id)
  (text-view (symbol->id id)
             (mtext-lookup id)
             large-text-size (layout 'fill-parent 'wrap-content -1 'centre 5)))

(define (mtitle-scale id)
  (text-view (symbol->id id)
             (mtext-lookup id)
             large-text-size (layout 'fill-parent 'wrap-content 1 'centre 5)))

(define (medit-text id type fn)
  (linear-layout
   (make-id (string-append (symbol->string id) "-container"))
   'vertical
   (layout 'fill-parent 'wrap-content 1 'centre 20)
   (list 0 0 0 0)
   (list
    (text-view    
     (make-id (string-append (symbol->string id) "-text"))
     (mtext-lookup id)
     normal-text-size (layout 'wrap-content 'wrap-content -1 'centre 0))
    (edit-text (symbol->id id) "" normal-text-size type
               (layout 'fill-parent 'wrap-content -1 'centre 0)
               fn))))

(define (medit-text-scale id type fn)
  (linear-layout
   (make-id (string-append (symbol->string id) "-container"))
   'vertical
   (layout 'fill-parent 'wrap-content 1 'centre 20)
   (list 0 0 0 0)
   (list
    (text-view      
     (make-id (string-append (symbol->string id) "-text"))
     (mtext-lookup id)
     normal-text-size (layout 'wrap-content 'wrap-content 1 'centre 0))
    (edit-text (symbol->id id) "" normal-text-size type
               (layout 'fill-parent 'wrap-content 1 'centre 0)
               fn))))

(define (medit-text-large id type fn)
  (linear-layout
   (make-id (string-append (symbol->string id) "-container"))
   'vertical
   (layout 'fill-parent 'wrap-content 1 'centre 20)
   (list 0 0 0 0)
   (list
    (text-view 0 (mtext-lookup id)
               normal-text-size (layout 'wrap-content 'wrap-content -1 'centre 0))
    (edit-text (symbol->id id) "" normal-text-size type
               (layout 'fill-parent 300 -1 'left 0)
               fn))))


(define (mspinner id types fn)
  (vert
   (text-view (symbol->id id)
              (mtext-lookup id)
              normal-text-size (layout 'wrap-content 'wrap-content 1 'centre 0))
   (spinner (make-id (string-append (symbol->string id) "-spinner"))
            (map mtext-lookup types)
            (layout 'wrap-content 'wrap-content 1 'centre 0)
            (lambda (c) (fn c)))))

(define (mspinner-other id types fn)
  (linear-layout
   (make-id (string-append (symbol->string id) "-container"))
   'horizontal
   (layout 'fill-parent 'wrap-content 1 'centre 5)
   (list 0 0 0 0)
   (list
    (vert
     (text-view (symbol->id id)
                (mtext-lookup id)
                normal-text-size 
		(layout 'wrap-content 'wrap-content 1 'centre 10))
     (spinner (make-id (string-append (symbol->string id) "-spinner"))
              (map mtext-lookup types)
              (layout 'wrap-content 'wrap-content 1 'centre 0)
              (lambda (c)
                ;; dont call if set to "other"
                (if (< c (- (length types) 1))
                    (fn c)
                    '()))))
    (vert
     (mtext-scale 'other)
     (edit-text (make-id (string-append (symbol->string id) "-edit-text"))
                "" normal-text-size "normal"
                (layout 'fill-parent 'wrap-content 1 'centre 0)
                (lambda (t) (fn t)))))))

(define (mspinner-other-vert id text-id types fn)
  (linear-layout
   0 'vertical
   (layout 'fill-parent 'wrap-content 1 'centre 5)
   (list 0 0 0 0)
   (list
    (text-view (symbol->id id)
               (mtext-lookup text-id)
               normal-text-size (layout 'wrap-content 'wrap-content 1 'centre 5))
    (spinner (make-id (string-append (symbol->string id) "-spinner"))
             (map mtext-lookup types)
             (layout 'wrap-content 'wrap-content 1 'centre 0)
             (lambda (c)
               ;; dont call if set to "other"
               (if (< c (- (length types) 1))
                   (fn c) '())))
    (mtext-scale 'other)
    (edit-text (make-id (string-append (symbol->string id) "-edit-text"))
               "" normal-text-size "normal"
               (layout 'fill-parent 'wrap-content 1 'centre 0)
               (lambda (t) (fn t))))))


(define (mclear-toggles id-list)
  (map
   (lambda (id)
     (update-widget 'toggle-button (get-id id) 'checked 0))
   id-list))

(define (mclear-toggles-not-me me id-list)
  (foldl
   (lambda (id r)
     (if (equal? me id)
         r (cons (update-widget 'toggle-button (get-id id) 'checked 0) r)))
   '() id-list))

(define (image-invalid? image-name)
  (or (null? image-name)
      (not image-name)
      (equal? image-name "none")
      (equal? image-name "")))

;; fill out the widget from the current entity in the memory store
;; dispatches based on widget type
(define (mupdate widget-type id-symbol key)
  (cond
   ((or (eq? widget-type 'edit-text) (eq? widget-type 'text-view))
    (let ((v (entity-get-value key)))
      ;;(msg "mupdate with" key " is number: " (number? v))
      (update-widget widget-type (get-symbol-id id-symbol) 'text
                     (cond
                      ;; hide -1 as it represents unset
                      ((and (number? v) (eqv? v -1)) "")
                      ((not v) "") ;; unset text
		      ((number? v) (number->string v))
                      (else v)))))
   ((eq? widget-type 'toggle-button)
    (update-widget widget-type (get-symbol-id id-symbol) 'checked
                   (entity-get-value key)))
   ((eq? widget-type 'image-view)
    (let ((image-name (entity-get-value key)))
      (if (image-invalid? image-name)
          (update-widget widget-type (get-symbol-id id-symbol) 'image "face")
          (update-widget widget-type (get-symbol-id id-symbol) 'external-image
                         (string-append dirname "files/" image-name)))))
   (else (msg "mupdate-widget unhandled widget type" widget-type))))

(define (spinner-choice l i)
  (if (number? i)
      (symbol->string (list-ref l i))
      i))

(define (symbol-list-to-names l)
  (map 
   (lambda (n) 
     ;; if it's a string already, use it - for custom values
     (if (string? n) n (mtext-lookup n)))
   l))

(define (mupdate-spinner id-symbol key choices)
  (let* ((val (entity-get-value key)))
    (if (not val)
        (update-widget 'spinner
                       (get-id (string-append (symbol->string id-symbol) "-spinner"))
                       'selection 0)
        (let ((index (index-find (string->symbol val) choices)))
          (if index
              (update-widget 'spinner
                             (get-id (string-append (symbol->string id-symbol) "-spinner"))
                             'selection index)
              (begin
                (msg "spinner item in db " val " not found in list of items")
                (update-widget 'spinner
                               (get-id (string-append (symbol->string id-symbol) "-spinner"))
                               'selection 0)))))))

(define (mupdate-spinner-other id-symbol key choices)
  (let* ((val (entity-get-value key)))
    (if (not val)
        (list (update-widget 'spinner
                             (get-id (string-append (symbol->string id-symbol) "-spinner"))
                             'selection 0))
        (let ((index (index-find (string->symbol val) choices)))
          (if index
              (list (update-widget 'spinner
                                   (get-id (string-append (symbol->string id-symbol) "-spinner"))
                                   'selection index))
              (list
               (update-widget 'spinner
                              (get-id (string-append (symbol->string id-symbol) "-spinner"))
                              'selection (- (length choices) 1))
               (update-widget 'edit-text
                              (get-id (string-append (symbol->string id-symbol) "-edit-text"))
                              'text val)))))))

;;;;
;; (y m d h m s)
(define (date-minus-months d ms)
  (let ((year (list-ref d 0))
        (month (- (list-ref d 1) 1)))
    (let ((new-month (- month ms)))
      (list
       (if (< new-month 0) (- year 1) year)
       (+ (if (< new-month 0) (+ new-month 12) new-month) 1)
       (list-ref d 2)
       (list-ref d 3)
       (list-ref d 4)
       (list-ref d 5)))))

(define (do-gps display-id key-prepend)
  (list
   (alert-dialog
    "gps-check"
    (mtext-lookup 'gps-are-you-sure)
    (lambda (v)
      (cond
       ((eqv? v 1)
        (list
         (alert-dialog
          "gps-check2"
          (mtext-lookup 'gps-are-you-sure-2)
          (lambda (v)
            (cond
             ((eqv? v 1)
              (let ((loc (get-current 'location '(0 0))))
                (entity-set-value! (string-append key-prepend "-lat") "real" (car loc))
                (entity-set-value! (string-append key-prepend "-lon") "real" (cadr loc))
                (list
                 (update-widget
                  'text-view
                  (get-id (string-append (symbol->string display-id) "-lat"))
                  'text
                  (number->string (car loc)))
                 (update-widget
                  'text-view
                  (get-id (string-append (symbol->string display-id) "-lon"))
                  'text
                  (number->string (cadr loc))))))
             (else '()))))))
       (else '()))))))

(define (mupdate-gps display-id key-prepend)
  (let ((lat (entity-get-value (string-append key-prepend "-lat")))
        (lon (entity-get-value (string-append key-prepend "-lon"))))
    (if (or (not lat) (not lon))
        (list
         (update-widget
          'text-view (get-id (string-append (symbol->string display-id) "-lat"))
          'text "O")
         (update-widget
          'text-view (get-id (string-append (symbol->string display-id) "-lon"))
          'text "0"))
        (list
         (update-widget
          'text-view (get-id (string-append (symbol->string display-id) "-lat"))
          'text (number->string lat))
         (update-widget
          'text-view (get-id (string-append (symbol->string display-id) "-lon"))
          'text (number->string lon))))))


;; a standard builder for list widgets of entities and a
;; make new button, to add defaults to the list - edit-activity
;; is called when the + button is pressed
(define (build-list-widget db table title title-ids entity-type edit-activity parent-fn ktv-default-fn)
    (vert-colour
     list-colour
     (horiz
      (mtitle-scale title)
      (button
       (make-id (string-append (symbol->string title) "-add"))
       "+" 40 (layout 100 'wrap-content 1 'centre 5)
       (lambda ()
         (let ((id (entity-create!
		    db table entity-type
		    (ktvlist-merge
		     (ktv-default-fn)
		     (list (ktv "parent" "varchar" (parent-fn)))))))
	   (list (start-activity edit-activity 0 id))))))
     
     (linear-layout
      (make-id (string-append entity-type "-list"))
      'vertical
      (layout 'fill-parent 'wrap-content 1 'centre 20)
      (list 0 0 0 0)
      (list))))

(define (build-list-widget-readonly db table title title-ids entity-type edit-activity parent-fn ktv-default-fn)
  (vert-colour
   list-colour
   (mtitle-scale title)
   (linear-layout
    (make-id (string-append entity-type "-list"))
    'vertical
    (layout 'fill-parent 'wrap-content 1 'centre 20)
    (list 0 0 0 0)
    (list))))

(define (make-list-widget-title e title-ids)
  (if (eqv? (length title-ids) 1)
      (ktv-get e (car title-ids))
      (string-append
       (ktv-get e (car title-ids)) "\n"
       (foldl
        (lambda (id r)
          (if (equal? r "")
              (ktv-get e id)
              (string-append r " " (ktv-get e id))))
        "" (cdr title-ids)))))

;; pull db data into list of button widgets
(define (update-list-widget db table title-ids entity-type view-activity parent)
  (let ((search-results
         (if parent
	     (db-filter-only db table entity-type
                             (list (list "parent" "varchar" "=" parent))
                             (map
                              (lambda (id)
                                (list id "varchar"))
                              title-ids))
             (db-all db table entity-type))))
    (update-widget
     'linear-layout
     (get-id (string-append entity-type "-list"))
     'contents
     (if (null? search-results)
         (list (mtext 'list-empty))
         (map
          (lambda (e)
            (button
             (make-id (string-append "list-button-" (ktv-get e "unique_id")))
             (make-list-widget-title e title-ids)
             button-text-size (layout 'fill-parent 'wrap-content 1 'centre 5)
             (lambda ()
               (list (start-activity view-activity 0 (ktv-get e "unique_id"))))))
          search-results)))))


(define (delete-button)
  (mbutton-scale
   'delete
   (lambda ()
     (list
      (alert-dialog
       "delete-check"
       (mtext-lookup 'delete-are-you-sure)
       (lambda (v)
         (cond
          ((eqv? v 1)
           (entity-set-value! "deleted" "int" 1)
           (entity-update-values!)
           (list (finish-activity 1)))
          (else
           (list)))))))))

(define (build-array-from-names db table entity-type)
  (map
   (lambda (e)
     (list (ktv-get e "name")
           (ktv-get e "unique_id")))
   (db-filter-only db table entity-type
                   (list)
                   (list (list "name" "varchar")))))

(define (find-index-from-name-array arr unique-id)
  (define (_ l i)
    (cond
     ((null? l) #f)
     ((equal? unique-id (cadr (car l))) i)
     (else (_ (cdr l) (+ i 1)))))
  (_ arr 0))

(define (safe-string->number str)
  (if (equal? str "") 0
      (string->number str)))


