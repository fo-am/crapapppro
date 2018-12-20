(define (msg . x) (display x)(newline))

(load "../decision.scm")
(load "../json.scm")
(load "../manure.scm")
(load "../crop-requirements.scm")
(load "../crop-tree-menu.scm")


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
  (display "choice->assoc")(newline)
  (display c)(newline)
  (display (car c))(newline)
  (list
   (cons "choice" (car c))
   (cons "value" (if (list? (cadr c))
                     (dtree->assoc (cadr c))
                     (cadr c)))))

(define (dtree->assoc d)
  (display "dtree->assoc")(newline)
  (display d)(newline)
  (display (car d))(newline)
  (list
   (cons "decision" (car d))
   (cons "choices" (map choice->assoc (cadr d)))))


(define (menutree->assoc d)
  (if (symbol? d)
      (symbol->string d)
      (list
       (cons "name" (car d))
       (cons "category" (cadr d))
       (cons "options" (map menutree->assoc (caddr d))))))

(define (spitout-json fn tree)
  (let ((f (open-output-file fn)))
    (display (scheme->json (dtree->assoc tree)) f)
    (close-output-port f)))

(define (spitout-json-raw fn tree)
  (let ((f (open-output-file fn)))
    (display (scheme->json (menutree->assoc tree)) f)
    (close-output-port f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spitout-json "manure.json" manure-tree)
(spitout-json "crop-requirements-n.json" crop-requirements-n-tree)
(spitout-json "crop-requirements-pk.json" crop-requirements-pk-tree)

(spitout-json-raw "crop-tree-menu.json" crop-tree-menu)
