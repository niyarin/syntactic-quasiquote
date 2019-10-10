(include "../src/syntactic-quasiquote.scm")

(import 
  (scheme base)
  (scheme write)
  (srfi 78) ;Lightweight testing
  (niyarin syntactic-quasiquote))


(define-syntax test-syntax
      (syntax-rules ()
         ((_ x)
          (syntactic-quasiquote
            x))))

(begin ;not use syntactic-unquote
   (check
     (test-syntax (quote ((1 (2 3) (quasiquote 5 6)))))
     =>
     '((1 (2 3) (quasiquote 5 6)))))

(begin;syntactic-lambda
  (check
     (test-syntax
       (syntactic-unquote
          ((syntactic-lambda (x) 
            (syntactic-car
               (syntactic-car x)))
           (syntactic-quote
             ((1 2)(3 4))))))
     => 1))


(begin;syntactic-cons
  (check
    (test-syntax
       (syntactic-unquote
         (syntactic-cons
           (syntactic-quote list)
           (syntactic-quote
             (1 2)))))
  => '(1 2)))
          

(begin;syntactic-cadr and syntactic-lambda
   (check
       (test-syntax
         (syntactic-unquote
           ((syntactic-lambda (syntactic-cadr)
               (syntactic-cadr (syntactic-quote (1 2 3 4 5))))
            (syntactic-lambda (y)
               (syntactic-car
                 (syntactic-cdr
                   y))))))
       => 2))

(begin;syntactic-equal?
  (check
    (test-syntax 
      (syntactic-unquote
         (syntactic-equal?
           1 1)))
    => #t)
  (check
    (test-syntax 
      (syntactic-unquote
         (syntactic-equal?
           1 2)))
    => #f)
  (check
    (test-syntax 
      (syntactic-unquote
         (syntactic-equal?
           (syntactic-quote (1 1))
           (syntactic-quote (1 1)))))
    => #t)
  )

(begin;IF
   (check
      (test-syntax
        (syntactic-unquote
          (syntactic-if
            (syntactic-equal? 1 1)
               (syntactic-quote 'ok)
               (syntactic-quote 'ng))))
      => 'ok)

   (check
      (test-syntax
        (syntactic-unquote
          (syntactic-if
            (syntactic-equal? 1 2)
               (syntactic-quote 'ok)
               (syntactic-quote 'ng))))
      => 'ng)
   )

(begin;MAP1
  (check
      (test-syntax
        (quote 
            (syntactic-unquote
              (syntactic-map1
                  syntactic-car
                  (syntactic-quote ((a 1)(b 2)))))))
      => '(a b))

   (check
      (test-syntax
        (quote 
            (syntactic-unquote
              (syntactic-map1
                  (syntactic-lambda (x)
                     (syntactic-car x))
                  (syntactic-quote ((a 1)(b 2)))))))
      => '(a b))
  )
(check-report)
