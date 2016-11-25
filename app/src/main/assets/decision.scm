;; Farm Crap App Pro Copyright (C) 2016 FoAM Kernow
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

;; a recursive decision tree that navigates via an unordered list
;; of choices

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
         (not (eqv? (length tree) 2))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit tests

(define (assert id p v)
  (when (not (eqv? p v)) (error "test" id "failed" p v)))

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
