(define-library (niyarin syntactic-quasiquote)
   (import (scheme base)
           (gauche base)
           (scheme write)
           )
   (export syntactic-quasiquote
           syntactic-car
           syntactic-cdr
           syntactic-equal?
           syntactic-cons
           syntactic-map1
           )
   (begin

     (define-syntax syntactic-car
         (syntax-rules (syntax-lambda)
           ((_ (syntax-lambda (cont-arg) cont-body) (a b ...))
            (let-syntax ((cont-syntax
                           (syntax-rules ()
                              ((_ cont-arg) cont-body))))
               (cont-syntax
                 a)))))

     (define-syntax syntactic-cdr
         (syntax-rules (syntax-lambda)
           ((_ (syntax-lambda (cont-arg) cont-body) (a b ...))
            (let-syntax ((cont-syntax
                           (syntax-rules ()
                              ((_ cont-arg) cont-body))))
               (cont-syntax
                 (b ...) )))))

     (define-syntax syntactic-cons
         (syntax-rules (syntax-lambda)
           ((_ (syntax-lambda (cont-arg) cont-body) a b)
            (let-syntax ((cont-syntax
                           (syntax-rules ()
                              ((_ cont-arg) cont-body))))
               (cont-syntax
                 (a . b) )))))

     (define-syntax syntactic-equal?
         (syntax-rules (syntax-lambda)
            ((_ (syntax-lambda (cont-arg) cont-body) a b)
             (let-syntax ((cont-syntax
                            (syntax-rules ()
                              ((_ cont-arg) cont-body))))
                (let-syntax ((%equal
                               (syntax-rules %... ()
                                 ((_ a) 
                                  (cont-syntax #t))
                                 ((_ v %...) 
                                  (cont-syntax #f)))))
                     (%equal b))))))

     (define-syntax %cps-syntactic-quasiquote-in-unquote-expand 
         (syntax-rules (syntax-lambda)
            ((_ continuation
                (res ...) 
                (obj1 obj2 ...))
             (%cps-syntactic-quasiquote-in-unquote
               (syntax-lambda (it)
                 (%cps-syntactic-quasiquote-in-unquote-expand
                   continuation
                   (res ... it)
                   (obj2 ...)))
               obj1))
            ((_ continuation
                (#(syntactic-fun (arg ...) body) res2 ...)
                ())
             (let-syntax ((syntax-fun 
                           (syntax-rules ()
                              ((_ arg ...) 
                               (%cps-syntactic-quasiquote-in-unquote
                                 continuation
                               body)))))
                            (syntax-fun (syntactic-quote res2) ...)))
            ((_ continuation
                (res1 res2 ...)
                ())
                 (res1 continuation res2 ...))))

     (define-syntax %cps-syntactic-quasiquote-in-unquote
         (syntax-rules (syntax-lambda syntactic-quote syntactic-lambda)
            ((_ (syntax-lambda (cont-arg) cont-body) (syntactic-quote  obj));QUOTE
             (let-syntax ((cont-syntax (syntax-rules () ((_ cont-arg) cont-body))))
                         (cont-syntax obj)))
            ((_ (syntax-lambda (cont-arg) cont-body)
                (syntactic-lambda (arg ...) body));LAMBDA
               (let-syntax ((cont-syntax
                                (syntax-rules ()
                                    ((_ cont-arg) cont-body))))
                 (cont-syntax 
                   #(syntactic-fun
                     (arg ...)
                     body))))
            ((_ continuation (syntactic-if "INTERNAL" #f true-case false-case));IF/FALSE
               (%cps-syntactic-quasiquote-in-unquote
                 continuation
                 false-case))

            ((_ continuation (syntactic-if  "INTERNAL" _ true-case false-case));IF/TRUE
               (%cps-syntactic-quasiquote-in-unquote
                 continuation
                 true-case))

            ((_ continuation (syntactic-if test true-case false-case))
             (%cps-syntactic-quasiquote-in-unquote
               (syntax-lambda (it)
                  (%cps-syntactic-quasiquote-in-unquote
                    continuation
                    (syntactic-if
                       "INTERNAL"
                       it
                       true-case
                       false-case)))
               test))
            ((_ continuation (obj ...))
             (%cps-syntactic-quasiquote-in-unquote-expand
               continuation
               ()
               (obj ...)))
            ((_ (syntax-lambda (cont-args) cont-body) obj)
             (let-syntax ((cont-syntax 
                            (syntax-rules ()
                              ((_ cont-args) cont-body))))
                  (cont-syntax
                    obj)))))

     (define-syntax %cps-syntactic-quasiquote-list
         (syntax-rules (syntactic-unquote syntax-lambda)
            ((_ continuation (syntactic-unquote obj)) ;unquote
             (%cps-syntactic-quasiquote-in-unquote
               continuation
                obj))

            ((_ continuation (obj1 obj2  ...))
             (%cps-syntactic-quasiquote-list continuation "INTERNAL" () (obj1 obj2 ...)))

            ((_ continuation
                "INTERNAL"
                (res ...)
                (obj1 obj2 ...))
             (%cps-syntactic-quasiquote
               (syntax-lambda (it)
                  (%cps-syntactic-quasiquote-list
                    continuation
                    "INTERNAL"
                    (res ... it)
                    (obj2 ...)))
               obj1))
            ((_ (syntax-lambda (cont-arg) cont-body)
                "INTERNAL"
                (res ...)
                ())
             (let-syntax ((cont-syntax
                            (syntax-rules ()
                              ((_ cont-arg) cont-body))))
               (cont-syntax
                 (res ...))))))

     (define-syntax %cps-syntactic-quasiquote
         (syntax-rules (syntax-lambda)
            ((_ (syntax-lambda (cont-arg) cont-body) (obj1 obj2  ...));LIST
             (%cps-syntactic-quasiquote-list
               (syntax-lambda (cont-arg) cont-body)
               (obj1 obj2 ...)))
            ((_ (syntax-lambda (cont-arg) cont-body) obj);ATOM
             (let-syntax ((cont-syntax (syntax-rules () ((_ cont-arg) cont-body))))
               (cont-syntax
                 obj)))))

     (define-syntax syntactic-quasiquote
         (syntax-rules ()
           ((_ body)
            (%cps-syntactic-quasiquote
              (syntax-lambda (it) it)
              body))))
     ))

