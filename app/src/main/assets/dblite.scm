;; Starwisp Copyright (C) 2013 Dave Griffiths
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

;; abstractions for synced databased

;; use:
;; (entity-init! db "table" "entity-type" (get-entity-by-unique ...))
;; (entity-set-value!)
;; (entity-update-values!)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stuff in memory

(msg "dblite.scm")

(define (store-set store key value)
  (cond
   ((null? store) (list (list key value)))
   ((eq? key (car (car store)))
    (cons (list key value) (cdr store)))
   (else
    (cons (car store) (store-set (cdr store) key value)))))

(define (store-get store key default)
  (cond
   ((null? store) default)
   ((eq? key (car (car store)))
    (cadr (car store)))
   (else
    (store-get (cdr store) key default))))

(define (store-exists? store key)
  (cond
   ((null? store) #f)
   ((eq? key (car (car store)))
    #t)
   (else
    (store-exists? (cdr store) key))))

(define store '())

(define (set-current! key value)
  (set! store (store-set store key value)))

(define (get-current key default)
  (store-get store key default))

(define (current-exists? key)
  (store-exists? store key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; db abstraction

;; entity set - for storing and adding to multiple entities in memory

(define (es-search es type)
  (cond
    ((null? es) #f)
    ((equal? (car (car es)) type) (car es))
    (else (es-search (cdr es) type))))

(define (es-set-entity es type ktv-list)
  (cond
    ((null? es) (list (list type ktv-list)))
    ((equal? (car (car es)) type) (cons (list type ktv-list) (cdr es)))
    (else (cons (car es) (es-set-entity (cdr es) type ktv-list)))))

(define es '())

(define (es-ktv-list)
  (let ((type (get-current 'entity-type #f)))
    (cond
     ((not type) (msg "es-ktv-list: no current entity type") '())
     (else
      (let ((s (es-search es type)))
        (cond
         ((not s) (msg "es-ktv-list: no entity for type " type) '())
         (else (cadr s))))))))

;; initialise the entity in memory - ktv-list can be empty for a new one
(define (entity-init! db table entity-type ktv-list)
  (set! es (es-set-entity es entity-type ktv-list))
  (set-current! 'db db)
  (set-current! 'table table)
  (set-current! 'entity-type entity-type))

;; init and immediately save the entity to the db
;; means it gets a unique_id
(define (entity-init&save! db table entity-type ktv-list)
  (entity-init! db table entity-type ktv-list)
  (let ((id (entity-create! db table entity-type ktv-list)))
    (entity-set-value! "unique_id" "varchar" id)
    id))

;; get value from current memory entity
(define (entity-get-value key)
  (ktv-get (es-ktv-list) key))

;; write value to memory entity
(define (entity-set-value! key type value)
  (set! es (es-set-entity
            es (get-current 'entity-type #f)
            (ktv-set (es-ktv-list) (ktv key type value)))))

(define (date-time->string dt)
  (string-append
   (number->string (list-ref dt 0)) "-"
   (substring (number->string (+ (list-ref dt 1) 100)) 1 3) "-"
   (substring (number->string (+ (list-ref dt 2) 100)) 1 3) " "
   (substring (number->string (+ (list-ref dt 3) 100)) 1 3) ":"
   (substring (number->string (+ (list-ref dt 4) 100)) 1 3) ":"
   (substring (number->string (+ (list-ref dt 5) 100)) 1 3)))

;; build new entity from all memory ktvs, insert to db, return unique_id
(define (entity-record-values!)
  (let ((db (get-current 'db #f))
        (table (get-current 'table #f))
        (type (get-current 'entity-type #f)))
    ;; standard bits
    (entity-create! db table type (es-ktv-list))))

;; used internally
(define (entity-create! db table entity-type ktv-list)
  ;(msg "creating:" entity-type ktv-list)
  (let ((values
         (append
          (list
           (ktv "user" "varchar" (get-current 'user-id "none"))
           (ktv "time" "varchar" (date-time->string (date-time)))
	   ;; no need for these!
	   ;;(ktv "lat" "real" (car (get-current 'location '(0 0))))
           ;;(ktv "lon" "real" (cadr (get-current 'location '(0 0))))
           (ktv "deleted" "int" 0))
          ktv-list)))
    ;(msg "about to insert")
    (let ((r (insert-entity/get-unique
              db table entity-type (get-current 'user-id "no id")
              values)))
      ;;(msg "entity-create: " entity-type)
      r)))

;; updates existing db entity from memory values
(define (entity-update-values!)
  (let ((db (get-current 'db #f))
        (table (get-current 'table #f)))
    ;; standard bits
    (let* ((values (es-ktv-list))
           (unique-id (ktv-get values "unique_id")))
      (cond
       ((and unique-id (not (null? values)))
        ;;(msg "entity-update-values inner" values)
        (update-entity db table (entity-id-from-unique db table unique-id) values)
        ;; removed due to save button no longer exiting activity - need to keep!
        ;;(entity-reset!)
        )
       (else
        (msg "no values or no id to update as entity:" unique-id "values:" values))))))

;; updates memory and writes a single value to the db
(define (entity-update-single-value! ktv)
  (entity-set-value! (ktv-key ktv) (ktv-type ktv) (ktv-value ktv))
  (let ((db (get-current 'db #f))
        (table (get-current 'table #f))
        (unique-id (ktv-get (es-ktv-list) "unique_id")))
    (cond
     (unique-id
      (update-entity db table (entity-id-from-unique db table unique-id) (list ktv)))
     (else
      (msg "no values or no id to update as entity:" unique-id)))))

;; local settings via the eavdb

(define settings-entity-id-version 1)

(define (get-setting-value name)
  (ktv-get (get-entity (get-current 'db #f) 
		       "local" settings-entity-id-version) name))

(define (set-setting! key type value)
  (update-entity
   (get-current 'db #f) 
   "local" settings-entity-id-version (list (ktv key type value))))

(define (db-delete-children db table type parent-id)
  (for-each
   (lambda (coord)
     (update-entity 
      db table 
      (entity-id-from-unique db table (ktv-get coord "unique_id")) 
      (list (ktv "deleted" "int" 1))))
   (db-filter 
    db table type
    (list
     (list "parent" "varchar" "=" parent-id)))))
