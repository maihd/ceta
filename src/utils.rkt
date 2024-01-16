#lang racket

;; Ceta programming languages by MaiHD

(provide (all-defined-out))

;; assert : any/c string? -> any/c
(define-syntax-rule (assert condition message)
  (if condition
      condition
      (error message)))

;; stack : (listof any/c) -> (listof any/c)
(define (stack . items)
  (reverse items))

;; stack? : any/c -> boolean?
(define (stack? a-stack)
  (list? a-stack))

;; stack-push : (listof any/c) -> (listof any/c)
(define (stack-push a-stack . items)
  (append (reverse items) a-stack))

;; stack-pop : (listof any/c) -> (listof any/c)
(define (stack-pop a-stack)
  (rest a-stack))

;; stack-pop : (listof any/c) -> any/c
(define (stack-get a-stack)
  (first a-stack))

;; arity-match : integer? -> void
(define (arity-match expect given)
  (when (not (= expect given))
    (error (string-append "arity mismatch. expect: "
                          (number->string expect)
                          " - given: "
                          (number->string given)))))

;; arity-min-match : integer? -> void
(define (arity-min-match expect given)
  (when (< given expect)
    (error (string-append "arity mismatch. at least "
                          (number->string expect)
                          " - given: "
                          (number->string given)))))

;; arity-max-match : integer? -> void
(define (arity-max-match expect given)
  (when (> given expect)
    (error (string-append "arity mismatch. at most "
                          (number->string expect)
                          " - given: "
                          (number->string given)))))