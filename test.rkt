#lang racket/load

(module a racket
  (require "generics.rkt")
  (provide plus f)
  
  (define-generic plus (x y))
  (define-method (plus [x (? number?)] [y (? number?)])
    (+ x y))
  
  (define (f x y) (plus x y)))

(module b racket
  (require rackunit
           "generics.rkt"
           'a)
  
  (define-method (plus [x (? string?)] [y (? string?)])
    (string-append x y))
  (check-equal? (f "foo" "bar")
                "foobar"))

(module c racket
  (require rackunit
           "generics.rkt"
           'a)
  
  (define-method (plus [x (? list?)] [y (? list?)])
    (append x y))
  (check-exn exn:fail? (lambda () (f "foo" "bar")))
  (check-equal? (f '(1 2 3) '(4 5 6))
                '(1 2 3 4 5 6))

  (struct point1 (x) #:transparent)
  (define-method (plus [p1 (struct point1)] [p2 (struct point1)])
    (point1 (+ (point1-x p1) (point1-x p2))))
  (check-equal? (plus (point1 3) (point1 5)) (point1 8)))

(require 'c)
(require 'b)
