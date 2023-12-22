#lang racket

(provide (all-defined-out)) ; export all top-level definitions

;;;;; Part 1: HOFs (and some utility functions)

(define (deep-map fn lst)
    (if (empty? lst)
      '()
      (if (list? lst)
          (cons (deep-map fn (first lst)) (deep-map fn (rest lst)))
          (if (pair? lst)
              (cons (deep-map fn (car lst)) (deep-map fn (cdr lst)))
              (fn lst)
              ))))


(define (my-curry fn . rest)
   (cond [(equal? (procedure-arity fn) 0)
          (lambda id (apply fn rest))]
         [(equal? (procedure-arity fn) (length rest))
          (apply fn rest)]
         [else
          (lambda id
            (apply my-curry fn (append rest id)))]))


(define (lookup key lst)
  (if (empty? (cdr lst))
      (if (equal? key (caar lst))
      (cdr (car lst))
      #f)
      (if (equal? key (caar lst))
      (cdr (car lst))
      (lookup key (cdr lst)))))


(define (update key value lst)
    (let rec ([returnList '()]
            [curList lst]
            [x 0])
    (if (empty? curList)
         (if (equal? x 1)
             (reverse returnList)
             (rec (cons (cons key value) returnList) '() 1))
         (if (equal? key (caar curList))
             (rec (cons (cons key value) returnList) (rest curList) 1)
             (rec (cons (car curList) returnList) (rest curList) x)
             ))))


(define (make-object name)
    (let rec ([atribute (cons (cons 'name name) '())])
    (lambda (rest . rst)
      (case rest
        ['get (lookup (first rst) atribute)]
        ['set (set! atribute (update (first rst)
                                     (first (cdr rst))
                                     atribute))]
        ['update (set! atribute (update (first rst)
                                        ((first (cdr rst)) (lookup (first rst) atribute))
                                        atribute))]
        ))))


;;;;; Part 2: Symbolic differentiation (no automated tests!)

;Check if exp is a sum, product or power (all of them with respective symbols +, *, ^)
(define (checkSum x) (and (pair? x) (eq? (car x) '+)))
(define (checkProduct x) (and (pair? x) (eq? (car x) '*)))
(define (checkPower x) (and (pair? x) (eq? (car x) '^)))

;Make Sum, Products or Power through lists

(define (makeSumProd x y) (list '+ x y))
(define (makeSum x y) ; Ignore first item (+)
  (let rec ([returnLst '(+)]
            [lst (rest y)])
    (if (empty? lst)
        (reverse returnLst)
        (rec (cons (diff x (first lst)) returnLst) (rest lst)))))
        
  
(define (makeProduct x y) (list '* x y))
(define (makePower x y) (list '^ x y))

(define (diff var exp)
    (cond
    [(number? exp) 0]
    
    [(symbol? exp) (if (eq? var exp) 1 0)]

    [(checkSum exp) (makeSum var exp)]
    
    [(checkProduct exp)
     (makeSumProd
      (makeProduct (cadr exp)
                     (diff var (caddr exp)))
      (makeProduct (diff var (cadr exp))
                     (caddr exp)))]
    [(checkPower exp)
     (makeProduct
      (caddr exp)
      (makePower (cadr exp) (sub1 (caddr exp))))]
    ))


;;;;; Part 3: Meta-circular evaluator

(define (my-eval rexp)
    (let my-eval-env ([rexp rexp]
                    [env '()])           ; environment (assoc list)
    (cond [(symbol? rexp)                ; variable
           (cdr (assoc rexp env))]
          [(eq? (first rexp) 'lambda)    ; lambda expression
           (lambda x (my-eval-env (third rexp) (cons (cons (car (second rexp)) (car x)) env)))]
          [else                          ; function application
           ((my-eval-env (first rexp) env) (my-eval-env (second rexp) env))])))


;;;;; Part 4: Free variables

(define (free sexp)
    (let my-free ([sexp sexp]               ; same approach as our meta-circular evaluator with env
                [env '()])
    (cond [(symbol? sexp)
           (if (lookup sexp env)
           '()                            ; this time if our lookup find we just ignore it
           (list sexp))]
          [(eq? (first sexp) 'lambda)
           (my-free (third sexp) (update (first (second sexp)) #t env))]
          [else (append (my-free (first sexp) env) (my-free (second sexp) env))]
   )))

;;;;; Extra credit: algebraic simplification (add your own tests)

;; Implemented features:
;; - ...
(define (simplify exp)
  (void))
