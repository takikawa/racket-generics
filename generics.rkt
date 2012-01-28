#lang racket

(require (for-syntax syntax/parse))

(provide define-generic
         define-method)

;; Data definitions

;; A dispatch table is a list of dispatch items
;; A dispatch item is a (dispatch-item list<any -> bool>
;;                                     (any ... -> any))
;;
;; TODO: what's the best representation of dispatch items?
;;  -- possibly a dispatch tree like in Tessman's "Adding Generic Functions to Scheme"
;;  -- how do you specify the order of overrides?
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
    (pattern (pred:expr param:id)))

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
