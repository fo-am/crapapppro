(define images
  (list
   (list 'cattle
         (list
          (list 25 "cattle_25m3")
          (list 30 "cattle_30m3")
          (list 50 "cattle_50m3")
          (list 100 "cattle_100m3")))
   (list 'FYM
         (list
          (list 25 "fym_25t")
          (list 50 "fym_50t")))
   (list 'pig
         (list
          (list 25 "pig_25m3")
          (list 50 "pig_50m3")
          (list 75 "pig_75m3")))
   (list 'poultry
         (list
          (list 5 "poultry_5t")
          (list 10 "poultry_10t")))
   (list 'compost
         (list
          (list 5 "compost_5t")
          (list 10 "compost_10t")
          (list 15 "compost_15t")
          (list 20 "compost_20t")
          (list 25 "compost_25t")
          (list 30 "compost_30t")
          (list 35 "compost_35t")
          (list 40 "compost_40t")
          (list 45 "compost_45t")
          (list 50 "compost_50t")))))

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
  (let ((type-images (_type images)))
    (if  (not type-images)
	 (begin
	   (msg "image not found for " type)
	   "cattle_25m3") ;; replace with blank image??
	 (cadr (_amount (cadr type-images) (car (cadr type-images)))))))

