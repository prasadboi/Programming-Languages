#lang plai-typed

(require "ps2-ast.rkt")

;; TODO: Implement the following two functions.
;;
;; parse should take an s-expression representing a program and return an
;; AST corresponding to the program.
;;
;; eval should take an expression e (i.e. an AST) and evaluate e,
;; returning the resulting value.
;;
;; See ps2-ast.rkt and README.md for more information.
(define (pair-up-exp (s : s-expression)) : (symbol * Expr)
  (let ([l (s-exp->list s)])
    (pair (s-exp->symbol (first l))
          (parse (second l)))))      

(define (parse (s : s-expression)) : Expr
  (
   cond
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-number? s) (valC (numV (s-exp->number s)))]
    [(s-exp-boolean? s) (valC (boolV (s-exp->boolean s)))]
    [(s-exp-list? s)
     (
      let [(l (s-exp->list s))]
       (
        case (s-exp->symbol (first l))
         [(+) (plusC (parse (second l)) (parse (third l)))]
         [(*) (timesC (parse (second l)) (parse (third l)))]
         [(natrec) (natrecC (parse (second l)) (parse (third l)) (s-exp->symbol (first (s-exp->list (fourth l)))) (s-exp->symbol (second (s-exp->list (fourth l)))) (parse (third (s-exp->list (fourth l)))) )]
         [(equal?) (equal?C (parse (second l)) (parse (third l)))]
         [(if) (ifC (parse (second l)) (parse (third l)) (parse (fourth l)))]
         [(list) (listC (map parse (rest l)))]
         [(cons) (consC (parse (second l)) (parse (third l)))]
         [(first) (firstC (parse (second l)))]
         [(rest) (restC (parse (second l)))]
         [(listrec) (listrecC (parse (second l)) (parse (third l)) (s-exp->symbol (first (s-exp->list (fourth l)))) (s-exp->symbol (second (s-exp->list (fourth l)))) (s-exp->symbol (third (s-exp->list (fourth l))))  (parse (fourth (s-exp->list (fourth l)))))]
         [(let) (letC (map pair-up-exp (s-exp->list (second l))) (parse (third l)))]
         [(let*) (let*C (map pair-up-exp (s-exp->list (second l))) (parse (third l)))]
         [(unpack) (unpackC (map s-exp->symbol (s-exp->list (second l))) (parse (third l)) (parse (fourth l)))]
       )
     )
    ]
  )
)

#|(define (subst (v1 : number) (x : symbol) (e : ArithE)) : ArithE
  (type-case ArithE e
             [idC (y) (if (equal? x y) (numC v1) (idC y))]
             [numC (n) (numC n)]
             [plusC (e1 e2) (plusC (subst v1 x e1) (subst v1 x e2))]
             [timesC (e1 e2) (timesC (subst v1 x e1) (subst v1 x e2))]
             [letC (y e1 e2)
                   (let ([e2 (if (equal? x y) e2 (subst v1 x e2))])
                     (letC y (subst v1 x e1) e2))
                   ]
             )
  )
|#
(define (check-symbol-exist-binding (s1 : symbol) (let-defs : (listof (symbol * Expr)))) : boolean
  (
   cond
    [(empty? let-defs) #f]
    [(equal? s1 (fst (first let-defs))) #t]
    [else (check-symbol-exist-binding s1 (rest let-defs))]
  )
)

(define (check-symbol-exist-list (s1 : symbol) (sym-defs : (listof symbol))) : boolean
  (
   cond
    [(empty? sym-defs) #f]
    [(equal? s1 (first sym-defs)) #t]
    [else (check-symbol-exist-list s1 (rest sym-defs))]
  )
)

(define (let*-defs-subst (v1 : Value) (s1 : symbol) (bindings : (listof (symbol * Expr)))) : (listof (symbol * Expr))
  (cond
  [(empty? bindings) (list)]
  [(equal? (fst (first bindings)) s1) ; in case same simply stop the substitution and return the rest of the list as is from here out
   (append (list (pair (fst (first bindings)) (subst v1 s1 (snd (first bindings))))) (rest bindings))
  ]
  [else (append (list (pair (fst (first bindings)) (subst v1 s1 (snd (first bindings))))) (let*-defs-subst v1 s1 (rest bindings)))])
)


(define (subst (v1 : Value) (s1 : symbol) (e : Expr)) : Expr
  (type-case Expr e
    [valC (v) e]
    [idC (x) (if (equal? x s1) (valC v1) (idC x))]
    [plusC (e1 e2) (plusC (subst v1 s1 e1) (subst v1 s1 e2))]
    [timesC (e1 e2) (timesC (subst v1 s1 e1) (subst v1 s1 e2))]
    [natrecC (e1 e2 x y e3) (natrecC (subst v1 s1 e1) (subst v1 s1 e2) x y (subst v1 s1 e3))]
    [equal?C (e1 e2) (equal?C (subst v1 s1 e1) (subst v1 s1 e2))]
    [ifC (guard e1 e2) (ifC (subst v1 s1 guard) (subst v1 s1 e1) (subst v1 s1 e2))]
    [listC (es) (listC (map (lambda (ei) (subst v1 s1 ei)) es))]
    [consC (e1 e2) (consC (subst v1 s1 e1) (subst v1 s1 e2))]
    [firstC (e) (firstC (subst v1 s1 e))]
    [restC (e) (restC (subst v1 s1 e))]
    [listrecC (e1 e2 hd restx res e3) (listrecC (subst v1 s1 e1) (subst v1 s1 e2) hd restx res (subst v1 s1 e3))]
    [letC (bindings e)
          (
           cond
            [(empty? bindings) (letC bindings (subst v1 s1 e))]
            [(equal? (check-symbol-exist-binding s1 bindings) #t) ; no substitutions to be made in e. only in ei
             (letC (map (lambda (bi) (pair (fst bi) (subst v1 s1 (snd bi)))) bindings) e)
            ]
            [else ; substitutions to be made both in bindings and e
             (letC (map (lambda (bi) (pair (fst bi) (subst v1 s1 (snd bi)))) bindings) (subst v1 s1 e))
            ]
          )
    ]
    [let*C (bindings e)
           (
            cond
             [(empty? bindings) (let*C bindings (subst v1 s1 e))]
             [(equal? (check-symbol-exist-binding s1 bindings) #t) (let*C (let*-defs-subst v1 s1 bindings) e)]
             [else (let*C (let*-defs-subst v1 s1 bindings) (subst v1 s1 e))]
           )
    ]
    
    [unpackC (vars e1 e2)
             (if (check-symbol-exist-list s1 vars) (unpackC vars (subst v1 s1 e1) e2)
                 (unpackC vars (subst v1 s1 e1) (subst v1 s1 e2))
             )
    ]
  )
)





(define (eval (e : Expr)) : Value
  (
   type-case Expr e
    ; Every value is a valid expression. Evaluating a value should just return the value.
    [valC (v) v]
    
    ; (+ e1 e2)
    ; Evaluates e1 and e2, which can be assumed to yield numbers n1 and n2, and returns their sum.
    [plusC (e1 e2) (numV (+ (numV-n (eval e1)) (numV-n (eval e2))))]

    ; (* e1 e2)
    ; Evaluates e1 and e2, which can be assumed to yield numbers n1 and n2, and returns their product.
    [timesC (e1 e2) (numV (* (numV-n (eval e1)) (numV-n (eval e2))))]

    ; (natrec e1 e2 (x y e3))
    ; Evaluates e1, which can be assumed to yield a non-negative integer n.
    ; - If n is 0, evaluates e2 and returns the result.
    ; - If n is > 0, first recursively executes (natrec (- n 1) e2 (x y e3)), to get some value v.
    ;   Then evaluates e3 with x bound to (- n 1) and y bound to v; the result of evaluating e3 is returned.
    ;
    ; E.g. evaluating (natrec 3 1 (x y (+ x y))) should return (numV 4)
    [natrecC (e1 e2 x y e3)
             (
              cond
               [(equal? (eval e1) (numV 0)) (eval e2)]
               [
                else (
                      let ([n-1 (eval (plusC (valC (eval e1)) (valC (numV -1))))])
                       (
                        let ([v (eval (natrecC (valC n-1) e2 x y e3))])
                         (eval (subst n-1 x (subst v y e3)))
                       )
                     )
               ]
             )
    ]

    ; (equal? e1 e2)
    ; Evaluates e1 and e2, and returns true if e1 is equal to e2, and returns false otherwise.
    [equal?C (e1 e2) (boolV (equal? (eval e1) (eval e2)))]

    ; (if guard e1 e2)
    ; Evaluates guard, which can be assumed to yield a boolean b.
    ; - If b is true, evaluates e1 and returns the result.
    ; - If b is false, evaluates e2 and returns the result.
    [ifC (guard e1 e2) (if (equal? (eval guard) (boolV #t)) (eval e1) (eval e2))]

    ; (list e1 e2 ... en)
    ; Evaluates e1 through en, yielding values v1 through vn, then returns the list containing v1 through vn
    [listC (es) (listV (map eval es))]

    ; (cons e1 e2)
    ; Evaluates e1 and e2, yielding v1 and v2 (which can be assumed to be a list),
    ; and returns the list that adds v1 to the front of the list represented by v2.
    [consC (e1 e2) (if (listV? (eval e2))  (listV (cons (eval e1) (listV-vs (eval e2)))) (error 'eval "Expected v2 to be a list!") )]

    ; (first e)
    ; Evaluates e yielding a value v (which can be assumed to be a non-empty list),
    ; and returns the first element of v.
    [firstC (e) (first (listV-vs (eval e)))]

    ; (rest e)
    ; Evaluates e yielding a value v (which can be assumed to be a non-empty list),
    ; and returns the tail of v.
    [restC (e) (listV (rest (listV-vs (eval e))))]

    ; (listrec e1 e2 (hd rest res e3))
    ; Evaluates e1 yielding a value v, which can be assumed to be a list.
    ; - If the list v is empty, evaluates e2 and returns the result.
    ; - If v is a non-empty list of the form (v1 v2 ... vn), first recursively
    ;   evaluates (listrec (v2 ... vn) e2 (hd rest res e3)) to get some value vrec
    ;   then evaluates e3 with hd bound to v1, rest bound to (v2 ... vn), and res bound to vrec.
    ;   Returns the result of evaluating e3.
    ;
    ; E.g. evaluating (listrec (list 1 2 3) 0 (x y res (+ x res))) should return (numV 6).
    ; (listrecC (listC (list (valC (numV 1)) (valC (numV 2)) (valC (numV 3)))) (valC (numV 0)) 'x 'y 'res (plusC (idC 'x) (idC 'res)))
    ; [listrecC (e1 : Expr) (e2 : Expr) (hd : symbol) (rest : symbol) (res : symbol) (e3 : Expr)]
    [
     listrecC (e1 e2 hd restx res e3)
     (
      type-case Value (eval e1)
       
      [listV (l)
       (
        cond
         [(empty? l) (eval e2)]
         [
          else
          (
           let ([vrec (eval (listrecC (valC (listV (rest l))) e2 hd restx res e3))])
           (eval (subst vrec res (subst (listV (rest l)) restx (subst (first l) hd e3))))
          )
         ]
       )
       ]
       [else (error 'eval "Expected (eval e1) to be a list!")]
     )
    ]
    
    ; (let ([x1 e1] [x2 e2] ... [xn en]) e)
    ; would be parsed as (letC (list (pair x1 e1) (pair x2 e2) ... (pair xn en)) e)
    ; Behaves similarly to the racket "let" form: e1 through en are evaluated to get values v1 through vn,
    ; then e is evaluated with xi bound to vi respectively.
    [letC (bindings e)
          (
           cond
            [(empty? bindings) (eval e) ]
            [else (eval (letC (rest bindings) (subst (eval (snd (first bindings))) (fst (first bindings)) e)))]
          )
    ]

    ; (let* ([x1 e1] [x2 e2] ... [xn en]) e)
    ; would be parsed as (let*C (list (pair x1 e1) (pair x2 e2) ... (pair xn en)) e)
    ; Behaves similarly to the racket "let*" form: e1 through en are evaluated to get values v1 through vn,
    ; then e is evaluated with xi bound to vi respectively.
    ; The difference between let is that in evaluating ei, the bindings for x1 through x_{i-1} are available.
    [let*C (bindings e)
          (
           cond
            [(empty? bindings) (eval e)]
            [
             else
             (
              eval (
                    let*C (map
                           (lambda
                             (bi)
                             (pair
                              (fst bi)
                              (subst (eval (snd (first bindings))) (fst (first bindings)) (snd bi))
                             )
                           )
                           (rest bindings)
                          )
                          (subst (eval (snd (first bindings))) (fst (first bindings)) e)
                   )
             )
            ]
          )
    ]


    ; (unpack (x1 x2 ... xn) e1 e2)
    ; would be parsed as (unpackC (list x1 x2 ... xn) e1 e2)
    ; Evaluates e1, which can be assumed to yield a list l of the form (v1 v2 ... vn)
    ; Then returns the result of evaluating e2 with x1 bound to v1, x2 bound to v2, ..., xn bound to vn.
    ; It is assumed that the list l has the same length as the length of the 'vars' argument.
    ; [unpackC (vars : (listof symbol)) (e1 : Expr) (e2 : Expr)]
    [unpackC (vars e1 e2)
             (
              let ([ls (eval e1)]) 
               (
                cond
                 [(listV? ls)
                  (
                   let ([l (listV-vs ls)])
                    (
                     cond
                      [(or (empty? l) (empty? vars)) (eval e2)]
                      [else
                       (eval (unpackC (rest vars) (listC (map (lambda (li) (valC li)) (rest l))) (subst (first l) (first vars) e2)))
                      ]
                    )
                  )
                 ]
                 [else (error 'eval "expected e2 to evaluate to a list!!!")]
               )
             )
    ]

    ; A use of a variable x would be parsed as (idC x).
    [idC (x) (error 'eval "unbound variable!")]
  )
  )