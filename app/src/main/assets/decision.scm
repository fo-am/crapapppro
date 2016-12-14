;; -*- mode: scheme; -*-
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

;; a choice is a name (single answer to the 'owner' tree) and a value
;; which can be another tree or a number, symbol etc
(define (choice name value) (list name value))
(define (choice-name p) (list-ref p 0))
(define (choice-value p) (list-ref p 1))

;; search a list of choices
(define (get-choice-value l name)
  (let ((r (assoc name l)))
    (if r (choice-value r) #f)))

;; a tree is a name (the question) and a list of choices
(define (dtree name choices) (list name choices))
(define (dtree-name d) (list-ref d 0))
(define (dtree-choices d) (list-ref d 1))

;; searches the choices in a tree for the one that matches, 
;; or returns the last one as default
(define (dtree-choose d name)
  (define (_ l)
    (cond
      ;; return the default (last) option if it's the only one left
      ((null? (cdr l))
       (when (not (eqv? (choice-name (car l)) name))
	     ;; issue a warning if it doesn't match
	     (msg "choosing default [" (car l) "] for" name))
       (choice-value (car l)))
      ;; it matches
      ((eqv? (choice-name (car l)) name)
       (choice-value (car l)))
      (else (_ (cdr l)))))
  (_ (dtree-choices d)))

;; does this look like a tree? (for debug)
(define (check-tree tree)
  (when (or
         (not (list? tree))
         (not (eqv? (length tree) 2))
        ;; (not (symbol? (car tree)))
         (not (list? (cadr tree))))
    (msg "not a tree:" tree)))
         
;; recursively search the tree based on the list of choices
(define (decision tree choices)
  (cond
    ((number? tree) tree) ;; we've reached a decision
    ((symbol? tree) tree) ;; we've reached a decision
    (else
     ;; for debug really...
     (check-tree tree)
     (let ((name (dtree-name tree))) ;; name from the tree
       ;; search for this in the supplied list
       (let ((value (get-choice-value choices name))) 
	 (when (not value) 
	       (msg "could not find tree decision" name "in supplied choices"))
	 (decision (dtree-choose tree value) choices))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit tests

(define (assert id p v)
  (when (not (eqv? p v)) (error "test" id "failed" p v)))

(define (test)
  (assert 1 (get-choice-value (quote ((one 1) (two 2))) 'two) 2)
  (assert 2 (get-choice-value (quote ((one 1) (two 2))) 'three) #f)
  (assert 3 (dtree-choose (quote (name ((one 1) (two 2) (default 77)))) 'two) 2) 
  (assert 4 (dtree-choose (quote (name ((one 1) (two 2) (default 77)))) 'three) 77) 
  (assert 5 (decision (quote (name ((one 1)  
				    (two (season
					  ((winter 3)
					   (default 39))))
				    (default 77))))
		      (quote ((name two)
			      (season spring)))) 39)
  (assert 6 (decision (quote (name ((one 1)
				    (two (season
					  ((winter 3)
					   (default 39))))
				    (default 77))))
		      (quote ((name two)
			      (season winter)))) 3)
  )

(test)
