(define (msg . x) (display x)(newline))

(load "decision.scm")
(load "json.scm")
(load "soil-nutrients.scm")


(define (scheme->json v)
  (cond
   ((number? v) (number->string v))
   ((symbol? v) (string-append "\"" (symbol->string v) "\""))
   ((string? v) (string-append "\"" v "\""))
   ((boolean? v) (if v "true" "false"))
   ((list? v)
    (cond
     ((null? v) "[]")
     (else
      ; if it quacks like an assoc list...
      (if (and (not (null? v)) (not (list? (car v))) (pair? (car v)))
          (assoc->json v)
          (list->json v)))))
   (else "[]"))) ;;(display "value->js, unsupported type for ") (display v) (newline) "[]")))

(define (list->json l)
  (define (_ l s)
    (cond
     ((null? l) s)
     (else
      (_ (cdr l)
         (string-append
          s
          (if (not (string=? s "")) ", " "")
          (scheme->json (car l)))))))
  (string-append "[" (_ l "") "]"))

; ((one . 1) (two . "three")) -> { "one": 1, "two": "three }

(define (assoc->json l)
  (define (_ l s)
    (cond
     ((null? l) s)
     (else
      (let ((token (scheme->json (car (car l))))
            (value (scheme->json (cdr (car l)))))
        (_ (cdr l) (string-append s (if (not (string=? s "")) "," "")
                                  "\n" token ": " value))))))
  (string-append "{" (_ l "") "\n" "}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (choice->assoc c)
  (list
   (cons "choice" (car c))
   (cons "value" (if (list? (cadr c))
                     (dtree->assoc (cadr c))
                     (cadr c)))))

(define (dtree->assoc d)
  (list
   (cons "decision" (car d))
   (cons "choices" (map choice->assoc (cadr d)))))

(define (spitout-json fn tree)
  (let ((f (open-output-file fn)))
    (display (scheme->json (dtree->assoc tree)) f)
    (close-output-port f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spitout-json "previous-grass-soil-nitrogen-supply-tree.json"
	      previous-grass-soil-nitrogen-supply-tree)
