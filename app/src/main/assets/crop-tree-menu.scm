;;#lang racket

(msg "crop-tree-menu")

(define (tree-menu name category values) (list name category values))
(define (tree-menu-name t) (list-ref t 0))
(define (tree-menu-category t) (list-ref t 1))
(define (tree-menu-options t) (list-ref t 2))

(define (tree-menu-get-options tree-menu)
  (map
   (lambda (value)
     (if (list? value) (tree-menu-name value) value))
   (tree-menu-options tree-menu)))

(define (tree-menu-find-submenu tree-menu name)
  (foldl
   (lambda (o r)
     (if (and (list? o) (eq? name (tree-menu-name o))) o r))
   #f
   (tree-menu-options tree-menu)))

(define (tree-menu-get-text-options tree-menu)
  (map 
   (lambda (v) 
     (if (number? v)
	 v (mtext-lookup v)))
   (tree-menu-get-options tree-menu)))

(define (tree-menu-option-is-submenu? tree-menu n)
  (list? (list-ref (tree-menu-options tree-menu) n))) 

(define (tree-menu-option-is-submenu? tree-menu n)
  (list? (list-ref (tree-menu-options tree-menu) n))) 

(define (menu-option category name) (list category name))
(define (menu-option-category o) (list-ref o 0))
(define (menu-option-name o) (list-ref o 1))

(define (tree-menu-navigate tree-menu options)
  (cond
    ;; no options yet - return the top level
    ((null? options) tree-menu)
    (else
     (let ((submenu (tree-menu-find-submenu
		     tree-menu (menu-option-name (car options)))))
       (cond
	(submenu (tree-menu-navigate submenu (cdr options)))
	(else
	 (msg "submenu not found" (menu-option-name (car options))
	      "in" tree-menu)
	 '()))))))


(define (tree-menu-option->string subtree-or-value)
  (cond 
   ((list? subtree-or-value)
    (mtext-lookup (tree-menu-name subtree-or-value)))
   ((symbol? subtree-or-value)	   
    (mtext-lookup subtree-or-value))
   ((number? subtree-or-value)
    (number->string subtree-or-value))
   ;; default to raw text - probably not needed
   (else subtree-or-value)))

;; for stuffing crop selection into a text field
(define (params-list->text params)
   (scheme->json params))

(define (text->params-list text)
  (let ((ret (json/parse-string text)))
    (if ret (map (lambda (e) 
		   (list (string->symbol (car e))
			 (if (number? (cadr e)) 
			     e (string->symbol (cadr e))))) 
		 ret) '())))

(define (rebuild-crop-select-list options)
  (update-widget 
   'linear-layout (get-id "crop-select-list") 'contents
   (map
    (lambda (option)
      (text-view 
       0 (mtext-lookup (menu-option-name option))
       normal-text-size
       (layout 'fill-parent 'wrap-content 1 'centre 5)))		  
    options)))

(define (update-tree-menu widget-id tree-menu current-options)
  (let ((current-tree (tree-menu-navigate tree-menu current-options)))
    (set-current! 'crop-menu-options current-options)
    (update-widget
     'linear-layout
     (get-id widget-id)
     'contents
     (map
      (lambda (subtree-or-value)
	(let ((option-string (tree-menu-option->string subtree-or-value)))
	  (button
	   (make-id (string-append "tree-menu-button-" option-string))			       
	   option-string
	   30 (layout 'fill-parent 'wrap-content -1 'centre 5)
	   (lambda () 
	     (cond
	      ;; recurse downward
	      ((list? subtree-or-value)
	       (list
		(update-widget 'linear-layout (get-id "crop-select-list") 'contents-add
			       (list
				(text-view 
				 0 (mtext-lookup (tree-menu-name subtree-or-value))
				 normal-text-size
				 (layout 'fill-parent 'wrap-content 1 'centre 5))))
		(update-widget 'text-view (get-id "crop-select-category") 'text
			       (mtext-lookup (tree-menu-category subtree-or-value)))	
		(update-tree-menu widget-id tree-menu 
				  (append
				   current-options
				   (list
				    (list (tree-menu-category current-tree)
					  (tree-menu-name subtree-or-value)))))))
	      (else ;; we are done
	       (set-current! 'crop-menu-options
			     (append
			      current-options
			      (list (list (tree-menu-category current-tree)
					  subtree-or-value))))
	       (msg (get-current 'crop-menu-options '()))

	       (entity-update-single-value! 
		(ktv "crop" "varchar" (params-list->text (get-current 'crop-menu-options '()))))
	       
	       (update-field-cropsoil-calc-from-current)
	       
	       (list (finish-activity 99))))))))
      (tree-menu-options current-tree)))))

(define crop-tree-menu
  (tree-menu
   'root 'crop
   (list

    (tree-menu
     'barley 'sown
     (list
      (tree-menu
       'spring 'application
       (list
        (tree-menu 'incorporated 'process (list 'feed 'malt))
        (tree-menu 'removed 'process (list 'feed 'malt))
        'wholecrop))
      (tree-menu
       'winter 'application
       (list
        (tree-menu 'incorporated 'process (list 'feed 'malt))
        (tree-menu 'removed 'process (list 'feed 'malt))
        'wholecrop))))

    'beetroot

    (tree-menu
     'brussels-sprout-cabbage 'subtype1
     (list
      'brussels-sprout
      'storage-cabbage
      'head-cabbage-pre-31-dec
      'head-cabbage-post-31-dec
      'collard-pre-31-dec
      'collard-post-31-dec))

    'bulb

    'carrot

    (tree-menu
     'cauliflower-calabrese 'subtype1
     (list
      'cauliflower-summer-autumn
      (tree-menu 
       'cauliflower-winter-hardy-roscoff 'subtype2
       (list 'seedbed 'top-dressing))
      'calabrese))


    'fodder-beet

    (tree-menu
     'grass 'subtype
     (list
      (tree-menu
       'established 'sown (list 'spring-sown 'summer-autumn 'clover))
      (tree-menu
       'grazed
       'targetyield
       (list 'DM4-5 'DM5-7 'DM6-8 'DM7-9 'DM9-12 'DM10-13 'DM12-15+))
      'hay
      'rye      
      (tree-menu
       'silage 'targetyield
       (list
        (tree-menu 'DM5-7 'cut (list 'one 'two 'three 'four))
        (tree-menu 'DM7-9 'cut (list 'one 'two 'three 'four))
        (tree-menu 'DM9-12 'cut (list 'one 'two 'three 'four))
        (tree-menu 'DM12-15+ 'cut (list 'one 'two 'three 'four))))))

    'kale-grazed

    (tree-menu
     'lettuce-leafy-salad 'subtype1
     (list 'lettuce-whole-head 'lettuce-baby-leaf 'wild-rocket))
    
    'linseed

    'maize

    (tree-menu
     'oat 'sown
     (list
      (tree-menu
       'winter 'application (list 'forage 'incorporated 'removed))
      (tree-menu
       'spring 'application (list 'forage 'incorporated 'removed))))

    (tree-menu
     'rape 'use
     (list 
      'forage 
      (tree-menu 
       'oilseed 'sown
       (list 'winter 'spring))))
    
    (tree-menu
     'onion-leek 'subtype1
     (list 'bulb-onion 'salad-onion 'leek))

    'pea-bean

    (tree-menu 
     'potato 'determinancy-group
     (list
      (tree-menu 
       'one 'growing-season-length
       (list '<60 '60-90 '90-120 '>120))
      (tree-menu 
       'two 'growing-season-length
       (list '<60 '60-90 '90-120 '>120))
      (tree-menu 
       'three 'growing-season-length
       (list '<60 '60-90 '90-120 '>120))
      (tree-menu 
       'four 'growing-season-length
       (list '<60 '60-90 '90-120 '>120))))
    
    (tree-menu
     'rye 'sown
     (list
      (tree-menu
       'winter 'application (list 'forage 'incorporated 'removed))
      (tree-menu
       'spring 'application (list 'forage 'incorporated 'removed))))

    (tree-menu
     'swede 'use
     (list 'forage-lifted 'forage-grazed 'vegetable))

    (tree-menu
     'triticale 'sown
     (list
      (tree-menu
       'winter 'application (list 'forage 'incorporated 'removed))
      (tree-menu
       'spring 'application (list 'forage 'incorporated 'removed))))

    (tree-menu
     'turnip 'use
     (list 'forage-lifted 'forage-grazed 'vegetable))

    (tree-menu
     'wheat 'sown
     (list
      (tree-menu
       'winter 'application
       (list
        (tree-menu 'incorporated 'process (list 'feed 'mill))
        (tree-menu 'removed 'process (list 'feed 'mill))
        'wholecrop))
      (tree-menu
       'spring 'application
       (list
        (tree-menu 'incorporated 'process (list 'feed 'mill))
        (tree-menu 'removed 'process (list 'feed 'mill))
        'wholecrop))))

    
     )))

