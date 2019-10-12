(define-library (niyarin syntactic-quasiquote)
   (import (scheme base))
   (export syntactic-quasiquote
           syntactic-car
           syntactic-cdr
           syntactic-equal?
           syntactic-cons
           syntactic-map1
           )
   (begin

     (define-syntax %apply-syntax-lambda
         (syntax-rules (syntax-lambda)
            ((_ (syntax-lambda (cont-args) cont-body) args ...)
             (let-syntax ((cont-syntax
                            (syntax-rules ()
                              ((_ cont-args) cont-body))))
                  (cont-syntax args ...)))))

     (define-syntax syntactic-car
         (syntax-rules (syntax-lambda)
           ((_ continuation (a b ...))
               (%apply-syntax-lambda 
                 continuation
                 a))))

     (define-syntax syntactic-cdr
         (syntax-rules (syntax-lambda)
           ((_ continuation (a b ...))
            (%apply-syntax-lambda 
              continuation
              (b ...) ))))

     (define-syntax syntactic-cons
         (syntax-rules (syntax-lambda)
           ((_ continuation a b)
            (%apply-syntax-lambda
              continuation
              (a . b)))))

     (define-syntax syntactic-equal?
         (syntax-rules (syntax-lambda)
            ((_ continuation a b)
                (let-syntax ((%equal
                               (syntax-rules %... ()
                                 ((_ a) 
                                  (%apply-syntax-lambda
                                    continuation
                                    #t))
                                 ((_ v %...) 
                                  (%apply-syntax-lambda
                                    continuation
                                    #f)))))
                     (%equal b)))))

     (define-syntax syntactic-map1
         (syntax-rules (syntax-lambda)
            ((_ continuation fun ())
             (%apply-syntax-lambda 
               continuation
               ()))
            ((_ continuation fun (a ...))
             (syntactic-map1 continuation "INTERNAL" () fun (a ...)))
            ((_ continuation "INTERNAL" (res ...) fun (a1 a2 ...))
             (%cps-syntactic-quasiquote-in-unquote
               (syntax-lambda (it)
                  (syntactic-map1
                    continuation
                    "INTERNAL"
                    (res ... it)
                    fun
                    (a2 ...)))
               (fun (syntactic-quote a1))))
            ((_ continuation "INTERNAL" (res ...) fun ())
             (%apply-syntax-lambda
               continuation
               (res ...)))))

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
            ((_ continuation (syntactic-quote  obj));QUOTE
               (%apply-syntax-lambda continuation obj))
            ((_ continuation
                (syntactic-lambda (arg ...) body));LAMBDA
                 (%apply-syntax-lambda
                   continuation
                   #(syntactic-fun
                     (arg ...)
                     body)))
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
            ((_ continuation obj)
                  (%apply-syntax-lambda
                    continuation
                    obj))))

     (define-syntax %cps-syntactic-quasiquote-list
         (syntax-rules (syntactic-unquote syntactic-unquote-splicing syntax-lambda)
            ((_ continuation (syntactic-unquote obj)) ;unquote
             (%cps-syntactic-quasiquote-in-unquote
               continuation
                obj))

            ((_ continuation (obj1 obj2  ...))
             (%cps-syntactic-quasiquote-list continuation "INTERNAL" () (obj1 obj2 ...)))

            ((_ continuation
                "INTERNAL" 
                (res ...)
                ((syntactic-unquote-splicing obj1) obj2 ...))
             (%cps-syntactic-quasiquote-in-unquote
               (syntax-lambda (args)
                  (let-syntax
                    ((ellipsis-expander
                       (syntax-rules %%... ()
                         ((_ (val %%...))
                           (%cps-syntactic-quasiquote-list
                             continuation
                             "INTERNAL"
                             (res ... val %%...)
                             (obj2 ...))))))
                    (ellipsis-expander
                      args)))
               obj1))

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
            ((_ continuation "INTERNAL" (res ...) ())
               (%apply-syntax-lambda
                 continuation
                 (res ...)))))

     (define-syntax %cps-syntactic-quasiquote
         (syntax-rules (syntax-lambda)
            ((_ (syntax-lambda (cont-arg) cont-body) (obj1 obj2  ...));LIST
             (%cps-syntactic-quasiquote-list
               (syntax-lambda (cont-arg) cont-body)
               (obj1 obj2 ...)))
            ((_ continuation obj);ATOM
               (%apply-syntax-lambda
                 continuation
                 obj))))

     (define-syntax syntactic-quasiquote
         (syntax-rules ()
           ((_ body)
            (%cps-syntactic-quasiquote
              (syntax-lambda (it) it)
              body))))
     ))

