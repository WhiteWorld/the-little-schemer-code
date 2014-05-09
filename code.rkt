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
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) 
                  (rember a (cdr lat)))))))
;firsts
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else 
       (cons (car (car l)) (firsts (cdr l)))))))

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
(member? 'meat '(mashed potatoes and meat gravy))
(member? 'liver '())
(or (eq? (car '(lox)) 'liver)
    (member? 'liver (cdr '(lox))))
(member? 'liver '(lox))
(or (eq? (car '(and lox)) 'liver)
    (member? 'liver (cdr '(low))))
(member? 'liver '(and lox))
(or (eq? (car '(bagels and lox)) 'liver)
    (member? 'liver '(bagels and lox)))
(member? 'liver '(bagels and lox))

;Cons the Magnificent
(rember 'bacon '(bacon lettuce and tomato))

;firsts
(firsts '((five plums) (four) (eleven green oranges)))

;define insertR
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons old 
             (cons new (cdr lat))))
      (else
       (cons (car lat)(insertR new old (cdr lat)))))))

;define insertL
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new lat))
      (else
       (cons (car lat)(insertR new old (cdr lat)))))))

(insertR 'topping 'fudge '(ice cream with fudge for dessert))
(insertR 'jalapeno 'and '(tacos tamales and salsa))
(insertR 'e 'd '(a b c d f g d h))   
(insertR 'a 'b '())

;define subst
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new (cdr lat)))
      (else
       (cons (car lat)
             (subst new old 
                    (cdr lat)))))))
(subst 'topping 'fudge '(ice cream with fudge for dessert))    

;define subst2
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat)'())
      (else(cond
             ((or (eq? o1 (car lat)) (eq? o2 (car lat)))
              (cons new (cdr lat)))
             (else
              (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))

;define multirember
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? a (car lat)) 
          (multirember a (cdr lat)))
         (else
          (cons (car lat) 
                (multirember a 
                             (cdr lat)))))))))
(multirember 'cup '(coffee cup tea cup and hick cup))

;define multiinsertR
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? '()))
      (else
       (cond
         ((eq? old (car lat))
          (cons (car lat)
                (cons new
                      (multiinsertR new old (cdr lat)))))
         (else
          (cons (car lat) multiinsertR(new old (cdr lat)))))))))

;define multiinsertL
(define multiinsertL
  (lambda (new old lat)
;;8


(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

; The Ninth Commandment



