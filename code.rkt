#lang racket
;define atom
(define atom?
  (lambda (a)
    (not (list? a))))

;Toys
;;Is an atom?
(quote atom)
(quote turkey)
(quote 1492)
(quote u)
(quote *abc$)
;;Is a list?
(quote (atom))
(quote (atom turkey or))
;;;(quote (atom turkey) or)
(quote ((atom turkey) or))
;;Is an S-expression?
(quote xyz)
(quote (x y z))
(quote ((x y) z))
(quote (how are you doing so far))
(quote (((how) are) ((you) (doing so)) far))
;Is a list?
(quote ())
(quote (() () ()))
;car of list(no-empty)
(car '(a b c))
(car '((a b c) x y z))
(car '(((hotdogs)) (and)(pickle)relish))
(car (car '(((hotdogs)) (and))))
;cdr of list(no-empty)
(cdr '(a b c))
(cdr '((a b c) x y z))
(cdr '(hamburger))
(cdr '((x) t r))
;car cdr
(car (cdr '((b)(x y)((c)))))
(cdr (cdr '((b)(x y)((c)))))
;(cdr (car '(a (b (c)) d)))

;cons (arguments: S-expression list
(cons 'peanut '(butter and jelly))
(cons '(banana and) '(peanut butter and jelly))
(cons '((help) this) '(is very ((hard)) to learn))
(cons '(a b (c)) '())

