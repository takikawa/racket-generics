#lang racket

(require (for-syntax racket/list
                     racket/struct-info
                     syntax/parse))

(provide define-generic
         define-method)

;; TODO: what's the best representation of dispatch items?
;;  -- possibly a dispatch tree like in Tessman's "Adding Generic Functions to Scheme"
;;  -- how do you specify the order of overrides?
;;
;;  One possibility:
;;    predicate language (ala the match macro) that allows
;;      arbitrary predicates (but no good reasoning for arbitrary cases)
;;    built-in reasoning about predicates in racket
;;
;;  support for specialized reasoning for structs and so on:
;;  e.g. (define-method (plus [p1 (struct point)] [p2 (struct point)])
;;         (struct (+ (point-x p1) (point-y p1))
;;                 (+ (point-x p1) (point-y p1))))
;;
;;       automatically uses the point? predicate and clauses can be
;;       ordered based on struct inheritance order.
;;
;;  Patterns like (? number?) and (? exact-positive-integer?) will be ordered
;;  in a built-in way. (? arbitrary-predicate?) will be invariant.

;; Data definitions
;;
;; A dispatch table is a list of dispatch items
;; A dispatch item is a (dispatch-item list<any -> bool>
;;                                     (any ... -> any))
;;
(define (make-dispatch-table) '())
(struct dispatch-item (preds body))

;; A generic contains a dispatch table and acts as the dispatcher
(struct generic ([table #:mutable] dispatcher)
        #:property prop:procedure
        (struct-field-index dispatcher))

;; (define-generic foo) binds
;;   foo to a generic containing table and dispatcher
;;   TODO: foo at phase 1 to allow some static checking
(define-syntax (define-generic stx)
  (syntax-parse stx
    [(_ name:id (par-name:id ... ))
     #'(define name (generic (make-dispatch-table)
                             (make-dispatcher name par-name ...)))]))

(define-syntax (define-method stx)
  (define-syntax-class param
    (pattern (param:id pat:predicate-pattern)
             #:with pred #'pat.pred))

  (define-syntax-class predicate-pattern
    (pattern ((~literal struct) name:id)
             #:when (struct-info? (syntax-local-value #'name))
             #:with pred
                    (third (extract-struct-info
                             (syntax-local-value #'name))))
    (pattern ((~literal ?) pred:expr)))

  (syntax-parse stx
    [(_ (name:id par:param ...) body)
     #'(let ([evaled-preds (list par.pred ...)])
         (set-generic-table!
          name
          (cons (dispatch-item evaled-preds (λ (par.param ...) body))
                (generic-table name))))]))

;; construct a dispatcher function
(define-syntax (make-dispatcher stx)
  (syntax-parse stx
    [(_ name:id par:id ...)
     #'(λ (par ...)
         (define the-table (generic-table name))
         (let loop ([table the-table])
           (when (null? table)
             (error "No default case provided."))
           (define item (first table))
           (if (andmap (λ (arg pred) (pred arg)) (list par ...) (dispatch-item-preds item))
               (apply (dispatch-item-body item) (list par ...))
               (loop (rest table)))))]))
