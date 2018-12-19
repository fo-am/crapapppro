;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert scheme values into equivilent json strings

;; two systems here first the original one, mangles symbols into
;; strings, used for communication with the java code.

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

(define (escape-quotes str)
  (foldl
   (lambda (c r)
     (if (equal? c #\")
         (string-append r "\\" (string c))
         (string-append r (string c))))
   ""
   (string->list str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; second system, goes both ways and maintains symbols properly

;; test func for env
(define (check) 42)

(define (tokenize)
  (let loop ((res '())
             (token (next-token)))
    (if (eof-object? token)
        (reverse res)
        (loop (cons token res) (next-token)))))

;;; helper for error reporting
(define (->string x)
  (cond
   ((char? x) (string x))
   ((symbol? x) (symbol->string x))
   ((eof-object? x) "<EOF>")
   (else "")))

;;; testing for control characters, handles only ascii and iso-8859-1 characters
(define (char-control? ch)
  (let ((i (char->integer ch)))
    (or (< i 32)
        (< 127 i 160))))

;; need to be able to gracefully handle errors for bw compat...
(define (lexer-error ch)
  #f)
  ;;(error (string-append "unexpected character " (->string ch))))

(define (parse-error token)
  #f)
;;(error (string-append "unexpected token " (->string token))))
;;; reads a character and signals an error if it does not match the expected character

(define (consume-char expect)
  (let ((ch (read-char)))
    (if (eqv? ch expect)
        ch
        (lexer-error ch))))

(define (next-token)
  (let ((ch (read-char)))
    (case ch
      ((#\space #\newline #\tab #\return) (next-token))
      ((#\" #\') (parse-string ch))
      ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (parse-number ch))
      ((#\[) 'open-brace)
      ((#\]) 'close-brace)
      ((#\{) 'open-curly)
      ((#\}) 'close-curly)
      ((#\:) 'colon)
      ((#\,) 'comma)
      ((#\t) (parse-true))
      ((#\f) (parse-false))
      ((#\n) (parse-null))
      (else (if (eof-object? ch)
                ch
                (lexer-error ch))))))

;;; XXX parsing of numbers is not really correct
(define (parse-number ch)
  (let loop ((res (string ch)))
    (let ((ch (peek-char)))
      (case ch
        ((#\- #\+ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\e #\E)
         (begin (read-char)
                (loop (string-append res (string ch)))))
        (else (string->number res))))))

;;; reads the symbol 'true, the first character is already read by next-token
(define (parse-true)
  (begin
    (consume-char #\r)
    (consume-char #\u)
    (consume-char #\e)
    #t))

;;; reads the symbol 'false the first character is already read by next-token
(define (parse-false)
  (begin
    (consume-char #\a)
    (consume-char #\l)
    (consume-char #\s)
    (consume-char #\e)
    #f))

;;; reads the symbol 'null the first character is already read by next-token
(define (parse-null)
  (begin
    (consume-char #\u)
    (consume-char #\l)
    (consume-char #\l)
    '()))

(define (check-symbol str)
  (if (eqv? (string-ref str 0) #\:)
      (string->symbol (substring str 1))
      str))

(define (parse-string q)
  (let loop ((res ""))
    (let ((ch (read-char)))
      (cond
       ((eqv? ch #\\) (loop (string-append res (string (parse-escape q)))))
       ((eqv? ch q) (check-symbol res))
       ((not (char-control? ch)) (loop (string-append res (string ch))))
       (else (lexer-error ch))))))

(define (parse-escape q)
  (let ((ch (read-char)))
    (case ch
      ((#\b) #\x08)
      ((#\f) #\x0C)
      ((#\n) #\newline)
      ((#\r) #\return)
      ((#\t) #\tab)
      ((#\\) #\\)
      ((#\u) (parse-unicode))
      (else (if (eqv? ch q)
                q
                (lexer-error ch))))))

(define (numeric-char-value ch)
  (- (char->integer ch) (char->integer #\0)))

(define (hex-char-value ch)
  (let ((i (char->integer ch)))
    (cond ((<= 97 i 102) (- i 87)) ; a-f
          ((<= 65 i 70) (- i 55)) ; A-F
          ((<= 48 i 57) (- i 48)) ; 0-9
          (else (lexer-error ch)))))

;;; parse an unicode escape consisting of four hexadecimal characters
(define (parse-unicode)
  (let* ((a (hex-char-value (read-char)))
         (b (hex-char-value (read-char)))
         (c (hex-char-value (read-char)))
         (d (hex-char-value (read-char))))
    (integer->char (+ (* 4096 a)
                      (* 256 b)
(* 16 c)
d))))

(define (parse-object)
  (parse-object-helper (next-token)))

(define (parse-object-helper token)
  (cond
   ((eqv? token 'open-curly) (parse-map))
   ((eqv? token 'open-brace) (parse-list))
   ((null? token) token)
   ((string? token) token)
   ((number? token) token)
   ((boolean? token) token)
   ((symbol? token) token)
   (else (parse-error token))))

(define (parse-map)
  (let loop ((res '(map)))
    (let ((token (next-token)))
      (cond
       ((eqv? token 'close-curly) (reverse res))
       ((string? token) (let* ((res (cons (cons (string->symbol token) (parse-map-value))
                                          res))
                               (next (next-token)))
                          (cond
                           ((eqv? next 'close-curly) (reverse res))
                           ((eqv? next 'comma) (loop res))
                           (else (parse-error next)))))
       (else (parse-error token))))))

(define (parse-map-value)
  (let ((token (next-token)))
    (if (eqv? token 'colon)
        (parse-object)
        (parse-error token))))

(define (parse-list)
  (let loop ((res '()))
    (let ((token (next-token)))
      (cond
       ((eqv? token 'close-brace) (reverse res))
       (else (let* ((res (cons (parse-object-helper token) res))
                    (next (next-token)))
               (cond
                ((eqv? next 'close-brace) (reverse res))
                ((eqv? next 'comma) (loop res))
                (else (parse-error next)))))))))

(define (double-quote s)
  (string-append "\"" s "\""))

(define (symbol-quote s)
  (string-append "\":" (symbol->string s) "\""))

(define (gen-string obj)
  (if (pair? obj)
      (string-append
       "[ "
       (foldl
        (lambda (o s)
          (string-append
           s
           (if ( > (string-length s) 0) ", " "")
           (gen-string o)))
        ""
        obj)
       " ]")
      (cond
       ((boolean? obj) (if obj "true" "false"))
       ((number? obj) (number->string obj))
       ((string? obj) (double-quote obj))
       ((symbol? obj) (symbol-quote obj))
       ((list? obj) "[]"))))

(define (with-input-from-string s p)
  (let ((inport (open-input-string s)))
    (if (eq? inport #f)
	#f
	(let ((prev-inport (current-input-port)))
	  (set-input-port inport)
	  (let ((res (p)))
	    (close-input-port inport)
	    (set-input-port prev-inport)
	    res)))))

;; JSON-API
(define (json/parse p)
  (with-input-from-port p parse-object))

(define (json/parse-string str)
  (with-input-from-string str parse-object))

(define (json/parse-file file)
  (with-input-from-file file parse-object))

(define json/gen-string gen-string)

