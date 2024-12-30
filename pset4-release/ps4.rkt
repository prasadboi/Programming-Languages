#lang plai-typed
(require "ps4-ast.rkt")
;; TODO: Implement the following two functions.
;;
;; parse should take an s-expression representing a program and return an
;; AST corresponding to the program.
;;
;; eval-base should take an expression e (i.e. an AST) and evaluate e,
;; returning the resulting value.
;;

;; See ps4-ast.rkt and README.md for more information.

;; Note that as in the previous problem set you probably want to implement a version
;; of eval that can return more general values and takes an environment / store as arguments.
;; Your eval-base would then be a wrapper around this more general eval that tries to conver the value
;; to a BaseValue, and fails if it cannot be converted.
;;
;; For grading, the test cases all result in values that can be converted to base values.
;; Value Type Definition
(define-type Value
  [noneV]
  [numV (n : number)]                      ;; Numbers
  [boolV (b : boolean)]                    ;; Booleans
  [closV (env : Env) (args : (listof symbol)) (body : Expr)] ;; Multi-arg closure
  [boxV (b : (boxof Value))]
  [objV (fields : Env) ;; Unevaluated field expressions
        (methods : (listof (symbol * Value))) ;; Methods are treated as multi arg closures
        (delegate : ((optionof Expr) * Env))])       ;; Optional delegate object

(define-type Binding
  [bind (name : symbol) (val : Value)])
(define-type-alias Env (listof Binding))
(define empty-env empty)
(define extend-env cons)

(define (lookup (x : symbol) (env : Env)) : Value
  (begin
    ;; (display "{LOOKUP: ")
    ;; (display "{SYMBOL: ")
    ;; (display x)
    ;; (display "}")
    ;; (display "{ENV: ")
    ;; (display env)
    ;; (display "}_______________________________")
    (cond
      [(empty? env) (error 'lookup "{{Variable not found}}")]
      [(equal? (bind-name (first env)) x) (begin
                                            ;; (display "{found the symbol in env!!!}_______________________________}}")
                                            (bind-val (first env)))]
      [else (lookup x (rest env))])))

(define (in-env? (x : symbol) (env : Env)) : boolean
  (cond
    [(empty? env) #f]
    [(equal? (bind-name (first env)) x) #t]
    [else (in-env? x (rest env))]))

;; -----------------------------------------PARSER----------------------------------------------------------
(define (parse (s : s-expression))
  :
  Expr
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-boolean? s) (boolC (s-exp->boolean s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))
            [(+) (plusC (parse (second l)) (parse (third l)))]
            [(*) (timesC (parse (second l)) (parse (third l)))]
            [(equal?) (equal?C (parse (second l)) (parse (third l)))]
            [(if) (ifC (parse (second l)) (parse (third l)) (parse (fourth l)))]
            [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
            [(lambda) (lambdaC (s-exp->symbol (second l)) (parse (third l)))]
            [(begin) (beginC (map parse (rest l)))]
            [(object)
             (let ([obj-fields (map (lambda (field)
                                      (let* ([f-list (s-exp->list field)]
                                             [f-name (s-exp->symbol (first f-list))]
                                             [f-val (parse (second f-list))])
                                        (pair f-name f-val)))
                                    (s-exp->list (second l)))]
                   [obj-methods (map (lambda (method)
                                       (let* ([m-list (s-exp->list method)]
                                              [m-name (s-exp->symbol (first m-list))]
                                              [m-args (map s-exp->symbol
                                                           (s-exp->list (second m-list)))]
                                              [m-body (parse (third m-list))])
                                         (method-decl m-name m-args m-body)))
                                     (s-exp->list (third l)))])
               (objectC (none) obj-fields obj-methods))]
            [(object-del)
             (let ([obj-fields (map (lambda (field)
                                      (let* ([f-list (s-exp->list field)]
                                             [f-name (s-exp->symbol (first f-list))]
                                             [f-val (parse (second f-list))])
                                        (pair f-name f-val)))
                                    (s-exp->list (third l)))]
                   [obj-methods (map (lambda (method)
                                       (let* ([m-list (s-exp->list method)]
                                              [m-name (s-exp->symbol (first m-list))]
                                              [m-args (map s-exp->symbol
                                                           (s-exp->list (second m-list)))]
                                              [m-body (parse (third m-list))])
                                         (method-decl m-name m-args m-body)))
                                     (s-exp->list (fourth l)))])
               (objectC (some (parse (second l))) obj-fields obj-methods))]
            [(msg)
             (msgC (parse (second l)) (s-exp->symbol (third l)) (map parse (rest (rest (rest l)))))]
            [(get-field) (get-fieldC (s-exp->symbol (second l)))]
            [(set-field!) (set-field!C (s-exp->symbol (second l)) (parse (third l)))]
            [else (appC (parse (first l)) (parse (second l)))])]
         [else (appC (parse (first l)) (parse (second l)))]))]))

;; -------------------------------helper functions-------------------------------------------------------

(define (build-method (m : MethodDecl) (f-env : Env)) : (symbol * Value)
  (let
      ([m-name (method-decl-name m)]
       [m-args (method-decl-args m)]
       [m-body (method-decl-body m)])
    (pair m-name (closV f-env m-args m-body))))

;; function to gather all the fields avaiable to a particular object
(define (build-field-env (obj : Value) (res : Env)): Env
  (;; an object has all of its fields stored as an environment
   ;; therefore we can simply add all the fields of the object. However, if a field definition of the object has already been defined earlier then we should not add our current object field to the env
   let* ([objfields (objV-fields obj)]
         [netfields #|this variable stores all the fields of the object that have not been overridden|#
          (filter (lambda (x) (not (in-env? (bind-name x) res))) objfields)]
         [newenv (append res netfields)])
    (begin
      ;; (display "{newenv}")
      ;; (display newenv)
    (type-case (optionof Expr) (fst (objV-delegate obj))
      [some (pexpr)
            (let ([pobj (eval-env (snd (objV-delegate obj)) pexpr)])
              (type-case Value pobj
                [objV (psym pfenv pd)
                      (build-field-env pobj newenv) ;; recursively call the function to add the fields of the parent into res
                      ]
                [else (error 'build-field-env "delegate expression didn't evaluate to type objV")]))]
      [none () newenv] ;; no more parents left to explore. simply return the environment
      ))))
;; function to merge two environments and keep the latest variable/field definitions
(define (merge-env (overriding-env : Env) (base-env : Env)) : Env
  (let ([modified-base-env (filter (lambda (x) (not (in-env? (bind-name x) overriding-env))) base-env)])
    (append overriding-env modified-base-env)))

;; function to build an environment from a list of symbols and their values
(define (build-env (symlist : (listof symbol)) (vallist : (listof Value)) (res : Env)) : Env
  ;; assuming that the symlist and vallist are of equal length
  (cond
    [(empty? symlist) res]
    [else (build-env (rest symlist) (rest vallist) (extend-env (bind (first symlist) (first vallist)) res))]))
;; function to search for a symbol through a list of bindings
(define (search-sym-val-bindings (s-name : symbol) (bindlist : (listof (symbol * Value)))) : (optionof Value)
  (cond
    [(empty? bindlist) (none)]
    [(equal? (fst (first bindlist)) s-name) (some (snd (first bindlist)))]
    [else (search-sym-val-bindings s-name (rest bindlist))]))

;; function that gets the method for a class
(define (get-method (obj : Value) (method-name : symbol)) : (optionof Value)
  (type-case Value obj
    [objV (f-list m-list d)
          (type-case (optionof Value) (search-sym-val-bindings method-name m-list)
            [some (m-val) ;; found the method!!
                  (begin
                    ;; (display "{Method Found: ")
                    ;; (display m-val)
                    ;; (display "}")
                    (some m-val))] 
            [none () ;; check it's parent methods (if any parents)
                  (type-case (optionof Expr) (fst d)
                    [some (d-expr)
                          (let ([p (eval-env (snd d) d-expr)])
                          (type-case Value p
                            [objV (p-f-list p-m-list p-d)
                                  (get-method p method-name)]
                            [else (error 'get-method "{parent is not of objV type}")]))]
                    [none () (none)])])]
    [else (error 'get-method "{you must provide an objV value as the first parameter}")]))
;; -----------------------------------------EVAL----------------------------------------------------------
(define (eval-env (env : Env) (e : Expr)) : Value
  (type-case Expr e
    [numC (n) (numV n)]
    [boolC (b) (boolV b)]
    [idC (x) (begin
               ;; (display "{idC}")
               (lookup x env))]
    [plusC (e1 e2)
           (begin
             ;; (display "plusC")
           (numV (+ (numV-n (eval-env env e1))
                    (numV-n (eval-env env e2)))))]
    [timesC (e1 e2)
            (begin
              ;;(display "{timesC}")
            (numV (* (numV-n (eval-env  env e1))
                     (numV-n (eval-env  env e2)))))]
    [equal?C (e1 e2)
             (begin
               ;; (display "{equal?C}")
             (boolV (equal? (eval-env env e1)
                            (eval-env env e2))))]
    [ifC (guard e1 e2)
         (begin
           ;; (display "{ifC}")
         (if (boolV-b (eval-env env guard))
             (eval-env env e1)
             (eval-env env e2)))]
    
    [letC (x e1 e2) (let ([v1 (eval-env env e1)]) (begin
                                                    ;; (display "{letC}")
                                                    (eval-env (extend-env (bind x v1) env) e2)))]
    
    [lambdaC (arg body)
             (begin
               ;; (display "{lambdaC}")
             (closV env (list arg) body))]
    
    [appC (func arg)
          (begin
          ;; (display "{appC}")
          (let
              ([v1 (eval-env env func)]
               [v2 (eval-env env arg)])
            (eval-env (extend-env (bind (first (closV-args v1)) v2) (closV-env v1)) (closV-body v1))))]

    [beginC (es)
            (begin
              ;; (display "{beginC}")
            (cond
              [(empty? (rest es)) (eval-env env (first es))]
              [else (let
                        ([curr-expr-eval (eval-env env (first es))])
                      (eval-env env (beginC (rest es))))]))]
    
    [objectC (delegate fields methods)
             (begin
               ;; (display "{objectC}")
             
             ;; load all the fields into the obj's field environemnt
             (let*
                 ([f-env (map (lambda (x) (bind (fst x) (boxV (box (eval-env env (snd x)))))) fields)])
               (objV f-env (map (lambda (m) (build-method m f-env)) methods)
                     (pair delegate env))))]
    
    [msgC (o method args)
          (begin
            ;; (display "{msgC}")
            ;; (display "{o : ")
            ;; (display o)
            ;; (display "}")
            ;; (display "{method: ")
            ;; (display method)
            ;; (display "}")
            ;; (display "{args: ")
            ;; (display args)
          (let
              ([obj (eval-env env o)])
            (type-case Value obj
              [objV (fieldenv methodlist d)
                    (let* ([field-append-env (merge-env (build-field-env obj empty-env) (snd d))]) ;; gather up all the fields that we can use for the msg
                      (type-case (optionof Value) (get-method obj method)
                        [some (m) ;; the method exists in the current object itself
                              (type-case Value m ;; the method is of type closV
                                [closV (fenv f-sym-arglist f-body)
                                       (let* ([f-args (begin
                                                        (rest f-sym-arglist))] ;; get all the symbol names for the input parameters to the function 
                                              [f-vals (begin
                                                        (map (lambda (x) (eval-env env x)) args))]
                                              [arg-env (build-env f-args f-vals empty-env)]
                                              [field-arg-append-env (merge-env arg-env field-append-env)]
                                              [app-env (begin
                                                         ;; (display "{||field-append-env: ")
                                                         ;; (display (in-env? 'x1 field-append-env))
                                                         ;; (display "||arg-env: ")
                                                         ;; (display arg-env)
                                                         ;; (display "}")
                                                         (cond
                                                         [(equal? #t (in-env? 'self env)) (begin
                                                                                            ;; (display "{'self is already in env. using lookup to get it}_______________________________")
                                                                                            (extend-env (bind 'self (lookup 'self env)) field-arg-append-env))]
                                                         [else (extend-env (bind 'self obj) field-arg-append-env)]))])
                                         (begin
                                           ;; (display "{msgC method: found and invoked")
                                           ;; (display (in-env? 'x1 app-env))
                                           ;; (display "}")
                                           ;; (display "{f-body: ")
                                           ;; (display f-body)
                                           ;; (display "}")
                                           (eval-env app-env f-body)))
                                       ]
                                [else (error 'eval-env "{Method doesn't follow designated structure as per program design}")])]
                        [none () (error 'eval-env "{Method does not exist in this or any of its parent objects}")]
                        ))
                    ]
              [else (error 'eval-env "{Did not get an object}")])))]

    [get-fieldC (name)
                (let* ([obj (lookup 'self env)]
                      [app-env (build-field-env obj empty-env)])
                  (begin
                    ;; (display "{get-field!C | field val is")
                    ;; (display (unbox (boxV-b (lookup name app-env))))
                    ;; (display "}")
                    (unbox (boxV-b (lookup name app-env)))))]
    [set-field!C (name e)
                 (let* ([obj (lookup 'self env)]
                        [app-env (merge-env (build-field-env obj empty-env) env)])
                  (begin
                    ;; (display "{set-field!C}")
                    (set-box! (boxV-b (lookup name app-env)) (eval-env app-env e))
                    (boolV #t)
                    ))]
    ))


;; --------------------------------------------------conv to eval-base----------------------------------------------------------
(define (conv-base-val (v : Value)) : BaseValue
  (cond
    [(numV? v) (numBV (numV-n v))]
    [(boolV? v) (boolBV (boolV-b v))]
    [else (error 'conv-base-val "Unknown value type not supported by BaseValue class")]
    )
  )
(define (eval-base (e : Expr)) : BaseValue
  (conv-base-val (eval-env empty-env e))
  )

;; ------------------------------------------testing----------------------------------------------------------------------------
(define ex0
  '(let o (object () ([get (self) 1]))
     (msg o get)))

(define ex1
  '(let o (object () ([plus (self x y) (+ x y)]))
     (msg o plus 10 5)))
;; (test-equal? "ex1" (eval-base (parse ex1)) (numBV 15))

(define ex2
  '(let o (object () ([plus (self x y) (+ x y)]
                      [minus (self x y) (+ x (* y -1))]))
     (msg o minus (msg o plus 10 5) 8)))
;; (test-equal? "ex2" (eval-base (parse ex2)) (numBV 7))

(define ex3
  '(let o (object () ([plus (self a b c d e f g h) (+ a (+ b (+ c (+ d (+ e (+ f (+ g h)))))))]))
     (msg o plus 1 2 3 4 5 6 7 8)))
;; (test-equal? "ex3" (eval-base (parse ex3)) (numBV 36))

(define ex4
  '(let o (object () ([get1 (self) 1]
                      [get2 (self) 2]
                      [combine (self) (+ (msg self get1) (msg self get2))]))
     (msg o combine)))
;; (test-equal? "ex4" (eval-base (parse ex4)) (numBV 3))

(define ex5
  '(let o (object ([x1 1] [x2 0])
                  ([setf (self v) (set-field! x1 v)]
                   [getf (self) (get-field x1)]))
     (begin (msg o setf 10)
            (msg o getf))))
;; (test-equal? "ex5" (eval-base (parse ex5)) (numBV 10))

(define ex6
  '(let myfun (lambda init
                (object ([x init])
                  ([setf (self v) (set-field! x v)]
                   [getf (self) (get-field x)])))
     (let o1 (myfun 10)
       (let o2 (myfun 11)
         (+ (msg o1 getf) (msg o2 getf))))))
;; (test-equal? "ex6" (eval-base (parse ex6)) (numBV 21))

(define ex7
  '(let o (object ([x 0])
                  ([incr (self) (set-field! x (+ 1 (get-field x)))]
                   [decr (self) (set-field! x (+ -1 (get-field x)))]
                   [get  (self) (get-field x)]))
     (begin (msg o incr)
            (msg o decr)
            (msg o incr)
            (msg o incr)
            (msg o get))))
;; (test-equal? "ex7" (eval-base (parse ex7)) (numBV 2))

(define ex8
  '(let parent
       (object () ([test1 (self) 1]))
     (let child
         (object-del parent () ([test2 (self) 5]))
       (+ (msg child test1) (msg child test2)))))
;; (test-equal? "ex8" (eval-base (parse ex8)) (numBV 6))

(define ex9
  '(let adder
       (object () ([val1 (self) 1]
                   [val2 (self) 2]
                   [add (self) (+ (msg self val1) (msg self val2))]))
     (let adder2
         (object-del adder () ([val1 (self) 10]))
       (msg adder2 add))))

;; (test-equal? "ex9" (eval-base (parse ex9)) (numBV 12))

(define ex10a
  '(let toggle
       (object ([status #f])
               ([switch (self) (set-field! status (if (get-field status) #f #t))]
                [get (self) (get-field status)]))
        (begin (msg toggle switch)
               (msg toggle switch)
               (msg toggle switch)
               (if (msg toggle get) 12 11))))
;; (test-equal? "ex10a" (eval-base (parse ex10a)) (numBV 12))

(define ex10b
  '(let mytrue
       (object () ([ifThen (self t f) t]))
    (let myfalse
        (object () ([ifThen (self t f) f]))
      (let toggle
          (object ([status myfalse])
                  ([switch (self) (set-field! status (msg (get-field status) ifThen myfalse mytrue))]
                   [get (self) (get-field status)]))
        (begin (msg toggle switch)
               (msg toggle switch)
               (msg toggle switch)
               (msg (msg toggle get) ifThen 2 1))))))
;; (test-equal? "ex10b" (eval-base (parse ex10b)) (numBV 2))

(define ex11
  '(let mk1Dpoint
       (lambda initx
         (object ([x initx])
                 ([set-x (self v) (set-field! x v)]
                  [get-x (self) (get-field x)]
                  )))
     (let mk2Dpoint
         (lambda initx
           (lambda inity
             (object-del (mk1Dpoint initx) ([y inity])
                     ([set-y (self v) (set-field! y v)]
                      [get-y (self) (get-field y)]))))
       (let p ((mk2Dpoint 1) 2)
         (begin (msg p set-x 127)
                (+ (msg p get-x) (msg p get-y)))))))

;; (test-equal? "ex11" (eval-base (parse ex11)) (numBV 129))


(define ex12
  '(let mkLeaf
       (lambda v
         (object () ([sum (self) v])))
     (let mkNode
         (lambda t1
           (lambda t2
             (object () ([sum (self) (+ (msg t1 sum) (msg t2 sum))]))))
       (let t1
           ((mkNode (mkLeaf 1)) (mkLeaf 2))
         (let t2
             ((mkNode (mkLeaf 10)) (mkLeaf 20))
           (let t
               ((mkNode t1) t2)
             (msg t sum)))))))
;; (test-equal? "ex12" (eval-base (parse ex12)) (numBV 33))

(define ex13
  '(let mkLeaf
       (lambda v
         (object ([val v]) ([sum (self) (get-field val v)]
                            [incr (self) (set-field! val (+ 1 (get-field val)))]
                            )))
     (let mkNode
         (lambda t1
           (lambda t2
             (object () ([sum (self) (+ (msg t1 sum) (msg t2 sum))]
                         [incr (self) (begin (msg t1 incr) (msg t2 incr))]
                         ))))
       (let t1
           ((mkNode (mkLeaf 1)) (mkLeaf 2))
         (let t2
             ((mkNode (mkLeaf 10)) (mkLeaf 20))
           (let t
               ((mkNode t1) t2)
             (begin (msg t incr)
                    (msg t incr)
                    (msg t sum))))))))
;; (test-equal? "ex13" (eval-base (parse ex13)) (numBV 41))

(define ex14
  '(let objectMaker
       (object ([numObj 1])
               ([mkObj (self)
                       (let count (get-field numObj)
                         (let o (object () ([getVal (self) count]))
                           (begin (set-field! numObj (+ 1 count))
                                  o)))]))
     (let o1 (msg objectMaker mkObj)
       (let o2 (msg objectMaker mkObj)
         (let o3 (msg objectMaker mkObj)
           (+ (msg o2 getVal) (msg o3 getVal)))))))
;; (test-equal? "ex14" (eval-base (parse ex14)) (numBV 5))

(define ex15
  '(let o
       (object ([f (lambda x x)])
               ([setf (self v) (set-field! f v)]
                [do1 (self v) ((get-field f) v)]
                [do2 (self v) ((get-field f) ((get-field f) v))]))
     (begin (msg o setf (lambda x (+ 1 x)))
            (+ (msg o do1 5) (msg o do2 10)))))
                
;; (test-equal? "ex15" (eval-base (parse ex15)) (numBV 18))

(define ex16
  '(let o
       (object ()
               ([doN (self f n v)
                     (if (equal? n 0)
                         v
                         (msg self doN f (+ -1 n) (f v)))]))
     (msg o doN (lambda x (* 2 x)) 4 1)))
                
;; (test-equal? "ex16" (eval-base (parse ex16)) (numBV 16))
