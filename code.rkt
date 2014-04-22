#lang racket
;define atom
(define atom?
  (lambda (a)
    (not (list? a))))
;define lambda
(define lat?
  (lambda (l)
          (cond
            ((null? l) #t)
            ((atom? (car l)) (lat? (cdr l)))
            (else #f))))

(define member?
  (lambda (a lat)
    (cond
      (null? #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))

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
(cons 'a '())
;(cons a b) -> (car (cons a b)) = a  (cdr (cons a b) = b
(cons 'a (car '((b) c d)))
(cons 'a (cdr '((b) c d)))

;null?  (defined only for lists, (null? a) is false for everything,except the empty list. 
(null? '());or (null? (quote ())); 
(null? '(a b c))
(null? 'spaghetti)
(atom? 'Harry)
(atom? '(Harry had a heap of apples))
(atom? (car '(Harry had a heap of apples)))
(atom? (cdr '(Harry had a heap of apples)))
(atom? (cdr '(Harry)))
(atom? (car (cdr '(swing low sweet cherry oat))))
(atom? (car (cdr '(swing (low sweet) cherry oat))))
(eq? 'Harry 'Harry)
(eq? 'margarine 'butter)
(eq? '() '(strawberry))
(eq? '6 '7)
;eq? takes two arguments. Must be a non-unmerbic atom.
(eq? (car '(Mary had a little lamb chop)) 'Mary)
(eq? (cdr '(soured milk)) 'milk)
(eq? (car '(beans beans we need jelly beans)) (car (cdr '(beans beans we need jelly beans))))

;Do it...
;lat
(lat? '(Jack Sprat could eat no chicken fat))
(lat? '((Jack) Sprat could eat no chicken fat))
(lat? '(Jack (Sprat could) eat no chicken fat))
(lat? '())

;or
(or (null? '()) (null? '(d e f g)))
(or (null? '(a b c)) (null? '()))
(or (null? '(a b c)) (null? '(atom)))
;member

(member? 'tea '(coffee tea or milk))
(member? 'poached '(fried eggs and scrambled eggs))
