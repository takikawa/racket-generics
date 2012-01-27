#lang racket

(require (for-syntax syntax/parse))

#|
(define-generic test)

(define-method (test (number? x) (number? y))
  "number")

(test "foo" "bar") ; => error

(define-method (test (string? x) (string? y))
  "string")

(test 3 5)         ; => "number"
(test "foo" "bar") ; => "string"
|#

;; Data definitions
(define (make-dispatch-table) '())
(struct dispatch-item (preds body))

;; intentional capture macro like (struct ...)
;; (define-generic foo foo-table) binds
;;   foo to a dispatcher
;;   foo-table to a dispatch table
;;   foo at phase 1 to generic function info
(define-syntax (define-generic stx)
  (syntax-parse stx
    [(_ name:id table-name:id (par-name:id ... ))
     #'(begin (define table-name (box (make-dispatch-table)))
              (define name (make-dispatcher table-name par-name ...)))]))

(define-syntax (define-method stx)
  (define-syntax-class param
    (pattern (pred:expr param:id)))
  
  (syntax-parse stx
    [(_ (name:id table-name:id par:param ...) body)
     #'(let ([evaled-preds (list par.pred ...)])
         (set! table-name
               (box (cons (dispatch-item evaled-preds (λ (par.param ...) body))
                          (unbox table-name)))))]))

;; (box dispatch-table?) -> procedure?
(define-syntax (make-dispatcher stx)
  (syntax-parse stx
    [(_ box-name:id par:id ...)
     #'(λ (par ...)
         (define the-table (unbox box-name))
         (let loop ([table the-table])
           (define item (first table))
           (if (andmap (λ (arg pred) (pred arg)) (list par ...) (dispatch-item-preds item))
               (apply (dispatch-item-body item) (list par ...))
               (loop (rest the-table)))))]))

;; examples
(define-generic foo foo-table (x y))
(define-method (foo foo-table (number? x) (number? y))
  (+ x y))
(define-method (foo foo-table (string? x) (string? y))
  (string-append x y))
(define-method (foo foo-table (string? x) (number? y))
  (+ (string->number x) y))