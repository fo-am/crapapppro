#lang racket

(define (param name value) (list name value))
(define (param-name p) (list-ref p 0))
(define (param-value p) (list-ref p 1))

(define (get-param-value l name)
  (let ((r (assoc name l)))
    (if r (param-value r) #f)))

(define (dtree-choice-name d) (list-ref d 0))
(define (dtree-choices d) (list-ref d 1))

(define (dtree-default-choice d)
  ;; return the last choice as default, should be raw value
  (let ((c (dtree-choices d)))
    (list-ref c (- (length c) 1))))

;; assumes choice name matches
(define (dtree-choose d name)
  (define (_ l)
    (cond
      ;; return the default (last) option
      ((null? (cdr l)) (param-value (car l)))
      ((eq? (param-name (car l)) name)
       (param-value (car l)))
      (else (_ (cdr l)))))
  (_ (dtree-choices d)))

(define (check-tree tree)
  (when (or
         (not (list? tree))
         (not (eq? (length tree) 2))
         (not (symbol? (car tree)))
         (not (list? (cadr tree))))
    (display "not a tree: ")(display tree)(newline)))
         
;; recursively
(define (decision tree params)
  (cond
    ((number? tree) tree) ;; we've reached a decision
    (else
     (check-tree tree)
     (let* ((name (dtree-choice-name tree))
            (value (get-param-value params name)))
       (when (not value) (display "could not find ")(display name)(display " in ")(display params)(newline))
       (decision (dtree-choose tree value) params)))))



(define tree
  '(type
    ((cattle 
      (quality 
       ((DM2 
         (nutrient 
          ((nitrogen
            (season		
             ((autumn
               (soil 
                ((sandyshallow (crop ((normal 8) (grass-oilseed 16))))
                 (mediumheavy (crop ((normal 48) (grass-oilseed 56)))))))
              (winter 48)
              (summer 72)
              (spring 56))))
           (phosphorous 30) 
           (potassium 220))))
        (DM6 
         (nutrient 
          ((nitrogen
            (season		
             ((autumn
               (soil 
                ((sandyshallow (crop ((normal 13) (grass-oilseed 26))))
                 (mediumheavy (crop ((normal 65) (grass-oilseed 78)))))))
              (winter 65)
              (summer 91)
              (spring 65))))
           (phosphorous 60) 
           (potassium 290))))
        (DM10
         (nutrient 
          ((nitrogen
            (season		
             ((autumn
               (soil 
                ((sandyshallow (crop ((normal 18) (grass-oilseed 36))))
                 (mediumheavy (crop ((normal 72) (grass-oilseed 90)))))))
              (winter 72)
              (summer 90)
              (spring 72))))
           (phosphorous 90) 
           (potassium 360)))))))
     
     (pig
      (quality
       ((DM2
         (nutrient 
          ((nitrogen
            (season		
             ((autumn
               (soil 
                ((sandyshallow (crop ((normal 15) (grass-oilseed 22.5))))
                 (mediumheavy (crop ((normal 52.5) (grass-oilseed 60)))))))
              (winter 60)
              (summer 82.5)
              (spring 82.5))))
           (phosphorous 25) 
           (potassium 90))))
        (DM4-pig
         (nutrient 
          ((nitrogen
            (season		
             ((autumn
               (soil 
                ((sandyshallow (crop ((normal 18) (grass-oilseed 27))))
                 (mediumheavy (crop ((normal 54) (grass-oilseed 63)))))))
              (winter 63)
              (summer 90)
              (spring 90))))
           (phosphorous 45) 
           (potassium 110))))
        (DM6-pig
         (nutrient 
          ((nitrogen
            (season		
             ((autumn
               (soil 
                ((sandyshallow (crop ((normal 22) (grass-oilseed 23))))
                 (mediumheavy (crop ((normal 55) (grass-oilseed 66)))))))
              (winter 69)
              (summer 99)
              (spring 99))))
           (phosphorous 65) 
           (potassium 125)))))))
     (poultry
      (quality
       ((layer
         (nutrient 
          ((nitrogen
            (season		
             ((autumn
               (soil 
                ((sandyshallow (crop ((normal 19) (grass-oilseed 28.5))))
                 (mediumheavy (crop ((normal 47.5) (grass-oilseed 57)))))))
              (winter 47.5)
              (summer 66.5)
              (spring 66.5))))
           (phosphorous 84) 
           (potassium 86)))))))
     (compost
      (quality
       ((green (nutrient ((nitrogen 7.5) (nitrogen-avail 0.2) (phosphorous 3) (potassium 5.5))))
        (green-food (nutrient ((nitrogen 11) (nitrogen-avail 0.6) (phosphorous 3.8) (potassium 8.0))))))))
    

    
    ))
        
     ;;(quality broiler (nitrogen (soil (crop 30 45) (crop 75 90)) (soil 60 75) 90 90) 150 162)))
   ;;(nutrients
   ;; FYM "tons/ha" 50
   ;; (list
   ;;  (quality other (nitrogen (soil 15 30) 30 30 30) 95 360) ;; other
   ;;  (quality fresh (nitrogen (soil 15 30) 30 45 30) 95 360) ;; soil inc fresh

(define (process-nutrients amount units quantity nutrients)
  (map
   (lambda (q)
     (rounding (* amount (/ q quantity))))
   nutrients))

(define (rounding a)
  (/ (round (* 10 a)) 10))

(define (get-nutrients type amount quality season crop soil)
  (let ((params (list (list 'type type) (list 'quality quality) (list 'season season) (list 'crop crop) (list 'soil soil))))
    (list
     (decision tree (append '((nutrient nitrogen)) params))
     (decision tree (append '((nutrient phosphorous)) params))
     (decision tree (append '((nutrient potassium)) params)))))


(get-nutrients 'cattle 20 'DM10 'summer 'normal 'sandyshallow)
(get-nutrients 'compost 20 'green 'none 'none 'none)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit tests

(define (assert id p v)
  (when (not (eq? p v)) (error "test" id "failed" p v)))

(define (test)
  (assert 1 (get-param-value '((one 1) (two 2)) 'two) 2)
  (assert 2 (get-param-value '((one 1) (two 2)) 'three) #f)
  (assert 3 (dtree-choose '(name ((one 1) (two 2) (default 77))) 'two) 2) 
  (assert 4 (dtree-choose '(name ((one 1) (two 2) (default 77))) 'three) 77) 
  (assert 5 (decision '(name ((one 1)
                              (two (season
                                    ((winter 3)
                                     (default 39))))
                              (default 77)))
                      '((name two)
                        (season spring))) 39)
  (assert 6 (decision '(name ((one 1)
                              (two (season
                                    ((winter 3)
                                     (default 39))))
                              (default 77)))
                      '((name two)
                        (season winter))) 3)
  )

(test)
(decision tree '((type cattle) (quality DM2) (nutrient nitrogen) (season autumn) (soil sandyshallow) (crop normal)))
