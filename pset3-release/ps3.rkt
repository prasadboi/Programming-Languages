#lang plai-typed
(require "ps3-ast.rkt")

;; TODO: Implement the following two functions.
;;
;; parse should take an s-expression representing a program and return an
;; AST corresponding to the program.
;;
;; eval-base should take an expression e (i.e. an AST) and evaluate e,
;; returning the resulting value.
;;

;; See ps3-ast.rkt and README.md for more information.

;; Note that as in lecture 6, you probably want to implement a version
;; of eval that returns a result that can be an arbitrary value (not just
;; a BaseValue) and also returns a store.  Your eval-base would then be a
;; wrapper around this more general eval that tries to conver the value
;; to a BaseValue, and fails if it cannot be.
;;
;; For grading, the test cases all result in values that can be converted to base values.

;; Value
(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [pairV (v1 : Value) (v2 : Value)]
  [closV (env : Env) (x : symbol) (e : Expr)]
  [boxV (l : Location)]
  [vectorV (vs : (listof Location))]
)

;; Location
(define-type-alias Location number)

;; Storage
(define-type Storage
  [cell (location : Location) (val : Value)])

;; Store
(define-type-alias Store (listof Storage))

;; empty store
(define empty-store empty)

;; override store
(define override-store cons)

;; Binding
(define-type Binding
  [bind (name : symbol) (loc : Location)])

;; Env
(define-type-alias Env (listof Binding))

;; Empty Env
(define empty-env empty)

;; Extend Env
(define extend-env cons)

;; fetch value from specified location in given store
(define (fetch (l : Location) (sto : Store)) : Value
  (cond
    [(cons? sto)
     (if (equal? (cell-location (first sto)) l)
         (cell-val (first sto))
         (fetch l (rest sto)))]
    [else (error 'fetch "No location found")]))

;; get the location of a symbol in a given env in the store
(define (lookup (x : symbol) (env : Env)) : Location
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-loc (first env))
         (lookup x (rest env)))]
    [else (error 'lookup "No binding found")]))

;; Result
(define-type Result
  [res (v : Value) (s : Store)])

;; new location
(define new-loc
  (let ([counter (box 0)])
    (lambda () 
      (let ([l (unbox counter)])
        (begin (set-box! counter (+ 1 l))
               l)))))

;; get the element at the ith index in the list
(define (get-vector-item (index : number) (l : (listof Location))) : Location
  (
     cond
      [(empty? l) (error 'get-list-item "Index out of bounds")]
      [(zero? index) (first l)]
      [else (get-vector-item (- index 1) (rest l))]
  )
)
  
;; get sublist of a list
(define (sublist (offset : number) (len : number) (l : (listof Location))) : (listof Location)
  (cond
    [(or (empty? l) (<= len 0)) empty]
    [(> offset 0) (sublist (- offset 1) len (rest l))]
    [else (cons (first l) (sublist 0 (- len 1) (rest l)))]
  )
)

;; parser
(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-boolean? s) (boolC (s-exp->boolean s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))
            [(pair) (pairC (parse (second l)) (parse (third l)))]
            [(fst) (fstC (parse (second l)))]
            [(snd) (sndC (parse (second l)))]
            [(+) (plusC (parse (second l)) (parse (third l)))]
            [(*) (timesC (parse (second l)) (parse (third l)))]
            [(equal?) (equal?C (parse (second l)) (parse (third l)))]
            [(if) (ifC (parse (second l)) (parse (third l)) (parse (fourth l)))]
            [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
            [(lambda) (lambdaC (s-exp->symbol (second l)) (parse (third l)))]
            [(box) (boxC (parse (second l)))]
            [(unbox) (unboxC (parse (second l)))]
            [(set-box!) (setboxC (parse (second l)) (parse (third l)))]
            [(vector) (vectorC (map parse (rest l)))]
            [(vector-length) (vector-lengthC (parse (second l)))]
            [(vector-ref) (vector-refC (parse (second l)) (parse (third l)))]
            [(vector-set!) (vector-set!C (parse (second l)) (parse (third l)) (parse (fourth l)))]
            [(vector-make) (vector-makeC (parse (second l)) (parse (third l)))]
            [(subvector) (subvectorC (parse (second l)) (parse (third l)) (parse (fourth l)))]
            [(begin) (beginC (map parse (rest l)))]
            [(transact) (transactC (parse (second l)))]
            [else (appC (parse (first l)) (parse (second l)))]
            )]
         [else (appC (parse (first l)) (parse (second l)))]
       ))]
    ))

;; evaluate
(define (eval-env (env : Env) (sto : Store) (e : Expr)) : Result
  (type-case Expr e
    
             [numC (n) (res (numV n) sto)]
    
             [idC (x) (res (fetch (lookup x env) sto) sto)]
    
             [boolC (b) (res (boolV b) sto)]
    
             [lambdaC (x e) (res (closV env x e) sto)]

             [pairC (e1 e2)
                    (type-case Result (eval-env env sto e1)
                      [res (v1 sto1)
                           (type-case Result (eval-env env sto1 e2)
                             [res (v2 sto2)
                                  (res (pairV v1 v2) sto2)])])]
    
             [fstC (e) (res (pairV-v1 (res-v (eval-env env sto e))) sto)]

             [sndC (e) (res (pairV-v2 (res-v (eval-env env sto e))) sto)]
    
             [plusC (e1 e2)
                    (type-case Result (eval-env env sto e1)
                      [res (v1 sto1)
                           (type-case Result (eval-env env sto1 e2)
                             [res (v2 sto2)
                                  (res (numV (+ (numV-n v1) (numV-n v2))) sto2)])])]
    
             [timesC (e1 e2)
                    (type-case Result (eval-env env sto e1)
                      [res (v1 sto1)
                           (type-case Result (eval-env env sto1 e2)
                             [res (v2 sto2)
                                  (res (numV (* (numV-n v1) (numV-n v2))) sto2)])])]

             [equal?C (e1 e2)
                      (type-case Result (eval-env env sto e1)
                        [res (v1 sto1)
                             (type-case Result (eval-env env sto1 e2)
                               [res (v2 sto2)
                                    (res (boolV (equal? v1 v2)) sto2)])])]
    
             [ifC (guard e1 e2)
                  (type-case Result (eval-env env sto guard)
                    [res (v1 sto1)
                         (cond
                           [(equal? #t (boolV-b v1))
                                   (eval-env env sto1 e1)]
                           [else
                            (eval-env env sto1 e2)])])]
    
             [letC (x e1 e2) (eval-env env sto (appC (lambdaC x e2) e1))]
    
             [appC (e1 e2)
                      (type-case Result (eval-env env sto e1)
                        [res (v1 sto-1)
                             (type-case Result (eval-env env sto-1 e2)
                                        [res (v2 sto-2)
                                             (let ([l (new-loc)])
                                               (eval-env
                                                (extend-env (bind (closV-x v1) l) (closV-env v1))
                                                (override-store (cell l v2) sto-2)
                                                (closV-e v1)))])])]
    
             
             [boxC (a) (type-case Result (eval-env env sto a)
                                  (res (v sto-1)
                                       (let [(l (new-loc))]
                                         (res (boxV l) (override-store (cell l v) sto-1)))))]
    
             [unboxC (a) (type-case Result (eval-env env sto a)
                                    (res (v sto-1)
                                         (res (fetch (boxV-l v) sto-1) sto-1)))]
    
             [setboxC (e1 e2)
                      (type-case Result (eval-env env sto e1)
                                 (res (v1 sto-1)
                                      (type-case Result (eval-env env sto-1 e2)
                                                 (res (v2 sto-2)
                                                      (res v2 (override-store (cell (boxV-l v1)
                                                                                    v2)
                                                                              sto-2))))))]
             [vectorC (es)
                      (cond
                        [(empty? es) (res (vectorV (list)) sto)]
                        [else
                         (type-case Result (eval-env env sto (first es))
                           (res (v1 sto1)
                                    (let* [(l (new-loc)) (eval-res (eval-env env sto1 (vectorC (rest es))))]
                                      (res (vectorV (cons l (vectorV-vs (res-v eval-res)))) (reverse (override-store (cell l v1) (reverse (res-s eval-res))))))))])]
    
             [vector-lengthC (e) (res (numV (length (vectorV-vs (res-v (eval-env env sto e))))) sto)]

             [vector-refC (e1 e2)
                          (type-case Result (eval-env env sto e1)
                            (res (v1 sto1)
                                 (type-case Result (eval-env env sto1 e2)
                                   (res (v2 sto2)
                                        (let* [(index (numV-n v2)) (vec (vectorV-vs v1)) (loc (get-vector-item index vec))]
                                          (res (fetch loc sto2) sto2))))))]

             [vector-set!C (e1 e2 e3)
                           (type-case Result (eval-env env sto e1)
                             (res (v1 sto1)
                                  (type-case Result (eval-env env sto1 e2)
                                    (res (v2 sto2)
                                         (type-case Result (eval-env env sto2 e3)
                                           (res (v3 sto3)
                                                (let* [(index (numV-n v2)) (vec (vectorV-vs v1)) (loc (get-vector-item index vec)) (newval v3)]
                                                  (res newval (override-store (cell loc newval) sto3)))))))))]

             [vector-makeC (e1 e2)
                           (type-case Result (eval-env env sto e1)
                             (res (v1 sto1)
                                  (type-case Result (eval-env env sto e2)
                                    (res (v2 sto2)
                                         (cond
                                           [(equal? (>= (numV-n v1) 0) #t) (
                                                                            let* [(len (numV-n v1))
                                                                                  (val v2)
                                                                                  (vec (build-list len (lambda (x) (new-loc))))
                                                                                  (sto3 (foldl (lambda (l s) (override-store (cell l val) s)) sto2 vec))
                                                                                  ]
                                                                             (res (vectorV vec) sto3)
                                                                            )]
                                           [else (error 'eval-env "The length of the vector to be made was less than 0. Hence not possible\n")])))))]
    
             [subvectorC (e offset len)
                         (type-case Result (eval-env env sto e)
                           (res (v1 sto1)
                                (type-case Result (eval-env env sto1 offset)
                                  (res (ofs sto2)
                                       (type-case Result (eval-env env sto2 len)
                                         (res (length sto3)
                                              (res (vectorV (sublist (numV-n ofs) (numV-n length) (vectorV-vs v1))) sto3)
                                              ))))))]

             [beginC (es)
                     (type-case Result (eval-env env sto (first es))
                       (res (v1 sto1)
                            (cond
                              [(empty? (rest es)) (res v1 sto1)]
                              [else (eval-env env sto1 (beginC (rest es)))]
                              )))]
    
             [transactC (e)
                        (type-case Result (eval-env env sto e)
                          (res (v sto1)
                               (let
                                   [(fval (pairV-v1 v)) (sval (pairV-v2 v))]
                                 (cond
                                   [(equal? (boolV-b fval) #t) (res sval sto1)]
                                   [else (res sval sto)]
                                 ))))])
  )

;; evalate the expression starting from an empty environment
(define (eval (e : Expr)) : Result
  (eval-env empty-env empty-store e))

;; convert value type to basevalue type
(define (conv-base-val (v : Value)) : BaseValue
  (cond
    [(numV? v) (numBV (numV-n v))]
    [(boolV? v) (boolBV (boolV-b v))]
    [(pairV? v) (pairBV (conv-base-val (pairV-v1 v)) (conv-base-val (pairV-v2 v)))]
    [else (error 'conv-base-val "Unknown value type not supported by BaseValue class")]
    )
)
;; evaluate-base wrapper
(define (eval-base (e : Expr)) : BaseValue
  (conv-base-val (res-v (eval e)))
  )