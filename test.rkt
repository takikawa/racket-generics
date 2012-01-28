#lang racket/load

(module a racket
  (require "generics.rkt")
  (provide plus f)
  
  (define-generic plus (x y))
  (define-method (plus (number? x) (number? y))
    (+ x y))
  
  (define (f x y) (plus x y)))

(module b racket
  (require "generics.rkt")
  (require 'a)
  
  (define-method (plus (string? x) (string? y))
    (string-append x y))
  (f "foo" "bar"))

(module c racket
  (require "generics.rkt")
  (require 'a)
  
  (define-method (plus (list? x) (list? y))
    (append x y))
  (f "foo" "bar")
  (f '(1 2 3) '(4 5 6)))

(require 'b)
(require 'c)