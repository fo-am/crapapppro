(define images
  (list
   (list 'cattle
         (list
	  (list 10 "cattle_slurry_10m3")
          (list 25 "cattle_25m3")
          (list 30 "cattle_slurry_30m3")
          (list 50 "cattle_50m3")
	  ;;(list 75 "cattle_slurry_75m3")
          (list 100 "cattle_100m3")))
   (list 'pig
         (list
          (list 10 "pig_slurry_10m3")
          (list 25 "pig_slurry_25m3")
          (list 30 "pig_slurry_30m3")
          (list 40 "pig_slurry_40m3")
          (list 50 "pig_slurry_50m3")))
   (list 'poultry
         (list
	  (list 2 "broiler_fym_2t")
	  (list 5 "broiler_fym_5t")
	  (list 7.5 "broiler_fym_7_5t")
	  (list 10 "broiler_fym_10t")
	  (list 15 "broiler_fym_15t")))	 
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
          (list 50 "compost_50t")))
   (list 'paper-crumble
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
          (list 50 "compost_50t")))
   (list 'spent-mushroom 
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
          (list 50 "compost_50t")))
   (list 'food-industry-waste
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
          (list 50 "compost_50t")))
   (list 'digestate-food
	 (list
	  (list 20 "digestate_20m3")
	  (list 30 "digestate_30m3")
	  (list 50 "digestate_50m3")
	  ))
   (list 'digestate-farm
	 (list
	  (list 20 "digestate_20m3")
	  (list 30 "digestate_30m3")
	  (list 50 "digestate_50m3")
	  ))
   ))

(define fym-images
  (list 
   (list 'fym-cattle
	 (list
	  (list 10 "cattle_fym_10t")
	  (list 15 "cattle_fym_15t")
	  (list 30 "cattle_fym_30t")
	  (list 75 "cattle_fym_75t")))
   (list 'fym-pig
	 (list
	  (list 5 "pig_fym_5t")
	  (list 10 "pig_fym_10t")
	  (list 15 "pig_fym_15t")
	  (list 20 "pig_fym_20t")
	  (list 25 "pig_fym_25t")
	  (list 30 "pig_fym_30t")))
   (list 'fym-sheep
	 (list
	  (list 10 "cattle_fym_10t")
	  (list 15 "cattle_fym_15t")
	  (list 30 "cattle_fym_30t")
	  (list 75 "cattle_fym_75t")))
   (list 'fym-duck
	 (list
	  (list 2 "broiler_fym_2t")
	  (list 5 "broiler_fym_5t")
	  (list 7.5 "broiler_fym_7_5t")
	  (list 10 "broiler_fym_10t")
	  (list 15 "broiler_fym_15t")))	 
   (list 'fym-horse
	 (list
	  (list 10 "cattle_fym_10t")
	  (list 15 "cattle_fym_15t")
	  (list 30 "cattle_fym_30t")
	  (list 75 "cattle_fym_75t")))
   (list 'fym-goat
	 (list
	  (list 10 "cattle_fym_10t")
	  (list 15 "cattle_fym_15t")
	  (list 30 "cattle_fym_30t")
	  (list 75 "cattle_fym_75t")))))

(define (find-image type quality amount)
  (define (_type images type)
    (cond
     ((null? images) #f)
     ((equal? (car (car images)) type) (car images))
     (else (_type (cdr images) type))))
  (define (_amount images s)
    (cond
     ((null? images) s)
     ((< (abs (- amount (car (car images))))
         (abs (- amount (car s))))
      (_amount (cdr images) (car images)))
     (else (_amount (cdr images) s))))
  (msg (symbol? type))
  (msg (string? type))
  (msg type)
  (if (eq? type 'fym)
      ;; switch to special fym list
      (let ((type-images (_type fym-images quality)))
	(if (not type-images)
	    (begin
	      ;;(msg "image not found for " type)
	      "cattle_25m3") ;; replace with blank image??
	    (cadr (_amount (cadr type-images) (car (cadr type-images))))))
      (let ((type-images (_type images type)))
	(if (not type-images)
	    (begin
	      ;;(msg "image not found for " type)
	      "cattle_25m3") ;; replace with blank image??
	    (cadr (_amount (cadr type-images) (car (cadr type-images))))))))

