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

;; a generic contains a dispatch table and acts as the dispatcher
(struct generic ([table #:mutable] dispatcher)
        #:property prop:procedure
        (struct-field-index dispatcher))

;; intentional capture macro like (struct ...)
;; (define-generic foo foo-table) binds
;;   foo to a dispatcher
;;   foo-table to a dispatch table
;;   foo at phase 1 to generic function info
(define-syntax (define-generic stx)
  (syntax-parse stx
    [(_ name:id (par-name:id ... ))
     #'(define name (generic (make-dispatch-table)
                             (make-dispatcher name par-name ...)))]))

(define-syntax (define-method stx)
  (define-syntax-class param
    (pattern (pred:expr param:id)))
  
  (syntax-parse stx
    [(_ (name:id par:param ...) body)
     #'(let ([evaled-preds (list par.pred ...)])
         (set-generic-table!
          name
          (cons (dispatch-item evaled-preds (λ (par.param ...) body))
                (generic-table name))))]))

;; (box dispatch-table?) -> procedure?
(define-syntax (make-dispatcher stx)
  (syntax-parse stx
    [(_ name:id par:id ...)
     #'(λ (par ...)
         (define the-table (generic-table name))
         (let loop ([table the-table])
           (define item (first table))
           (if (andmap (λ (arg pred) (pred arg)) (list par ...) (dispatch-item-preds item))
               (apply (dispatch-item-body item) (list par ...))
               (loop (rest the-table)))))]))

;; examples
(define-generic foo (x y))
(define-method (foo (number? x) (number? y))
  (+ x y))
(define-method (foo (string? x) (string? y))
  (string-append x y))
(define-method (foo (string? x) (number? y))
  (+ (string->number x) y))