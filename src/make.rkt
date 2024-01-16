#lang racket

;;; Ceta programming languages by MaiHD

(struct make-expr () #:transparent)
(struct make-def-expr make-expr (name) #:transparent)
(struct make-defun-expr make-def-expr (dependencies body) #:transparent)
(struct make-defvar-expr make-def-expr (definition) #:transparent)
(struct make-exec-expr make-expr (name arguments) #:transparent)

(struct make-atom-expr make-expr (data) #:transparent)
(struct make-number-expr make-atom-expr () #:transparent)
(struct make-string-expr make-atom-expr () #:transparent)
(struct make-symbol-expr make-atom-expr () #:transparent)

(define-syntax define-make-parser
  (syntax-rules ()))