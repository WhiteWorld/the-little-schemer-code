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
    (cond
      ((null? '()))
      (else
       (cond
         ((eq? old (car lat))
          (cons new
                (cons old
                      (multiinsertL new old (cdr lat)))))
         (else
          (cons (car lat) multiinsertL(new old (cdr lat)))))))))

;define multisubst
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? old (car lat))
          (cons new 
                (multisubst new old (cdr lat))))
         (else
          (cons (car lat) 
                (multisubst new old (cdr lat)))))))))

;Numbers Games
(atom?  14)
(atom? -3)
(atom? 3.14159)
(+ 67 1)
;define add1
(define add1
  (lambda (n)
    (+ n 1)))
(add1 67)
;define sub1
(define sub1
  (lambda (n)
    (- n 1)))
(sub1 5)
(sub1 0)
(zero? 0)
(zero? 1492)
(- 14 3)
(- 17 9)
(- 18 25)

;define -
#|
(define -
  (lambda (a b)
    (cond
    ((zero? a) b)
    (else (sub1 (- a (sub1 b)))))))
|#

;tuple (a list of numbers)

;define addtup
(define addtup
  (lambda (tup)
    (cond
      ((null? tup)0)
      (else (+ (car tup) (addtup (cdr tup)))))))
(addtup '(1 3 4))
;define tup+
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1)(null? tup2)) '())
      (else
       (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(1 2) '(3 4))

;define length
(define length
  (lambda (lat)
    (cond
      ((null? lat)0)
      (else (add1 (length (cdr lat)))))))
(length '(1 2 3))

;define pick
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
(pick 3 '(aa bb cc dd))

;define rempick
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 3 '(hotdogs with hot mustard))

(number? 'a)
(number? 12)

;define no-nums
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat)'())
      (else
       (cond
       ((number? (car lat)) (no-nums (cdr lat)))
       (else
       (cons (car lat) (no-nums (cdr lat)))))))))
(no-nums '(5 pears 6 prunes 9 dates))

;define all-nums
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat))
      (else
       (cond
         ((number?(car lat))
          (cons (car lat)
                (all-nums (cdr lat))))
         (else (all-nums (cdr lat))))))))

;define eqan
(define eqan
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))
;occur
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eq? (car lat))
          (add1 (occur a (cdr lat))))
         (else (occur a (cdr lat))))))))
;define one
(define one?
  (lambda (n)
      (= n 1)))
      
(one? 3)

;define rempick1
(define rempick1
  (lambda (n lat)
    (cond
      ((one? n)(cdr lat))
      (else
       (cons (car lat) (rempick1 (sub1 n) (cdr lat)))))))
(rempick1 3 '(lemon meringue salty pie))

;5
;define rember*
(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
        ((eq? (car l) a)
         (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
       (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(lat? '((tomato sauce) ((bean)sauce)(and ((flying)) sauce)))

;define insertR*
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l)'())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons old
                (cons new 
                      (insertR* new old
                                (cdr l)))))
         (else (cons (car l) 
                     (insertR* new old
                               (cdr l))))))
      (else (cons (insertR* new old 
                            (car l))
                  (insertR* new old
                            (cdr l)))))))
;define occur*
(define occur*
  (lambda (a l)
    (cond
      ((null? l)0)
      ((atom? (car l))
              (cond
                ((eq? a (car l))
                 (add1 (occur* a (cdr l))))
                (else
                 (occur* a (cdr l)))))
       (else
        (+  (occur* a (car l))
             (occur* a (cdr l)))))))

;define subst*
(define subst*
  (lambda (new old l)
    (cond
      ((null? l)'())
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons new (subst* new old (cdr l))))
         (else
          (cons (car l) (subst* new old (cdr l))))))
       (else
        (cons 
         (subst* new old (car l)) 
         (subst* new old (cdr l)))))))
;insertL*
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l)'())
      ((atom?(car l))
       (cond
         ((eq? old (car l))
          (cons new 
                (cons old 
                      (insertL* new old (cdr l)))))
         (else (cons (car l) 
                     (insertL* new old (cdr l))))))
       (else
        (cons
         (insertL* new old (car l))
         (insertL* new old (cdr l)))))))

;member*
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
       ((atom? (car l))
        (cond
          ((eq? a (car l)) #t)
          (else (member* a (cdr l)))))
       (else
        (or (member* a (car l)) (member* a (cdr l)))))))
               
;leftmost 
(define leftmost
  (lambda (l)
    (cond
    ((atom? (car l)) (car l))
    (else
     (leftmost (car l))))))

;define eqlist
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eq? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else
       (and
        (eqlist? (car l1) (car l2))
        (eqlist? (cdr l1) (cdr l2)))))))
      
       
      
      
      
(eqlist? '(strawberry ice cream) '(strawberry ice cream)) 


;;6
