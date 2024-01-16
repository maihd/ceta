#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ceta programming languages by MaiHD
;;; 
;;; Preprocessor system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))

;; require parser for parsing syntax
(require "parser.rkt" "ast.rkt" "utils.rkt")

(struct macro-expander (name macro) #:transparent)

(define macro-expanders (list))

;; install-macro-expander
(define (install-macro-expander name macro)
  (when (has-macro-expander? name)
    (error "attempt to redefine macro"))
  (set! macro-expanders (cons (macro-expander name macro)
                              macro-expanders)))

;;
(define (has-macro-expander? name)
  (member #true (map (lambda (x) (equal? name (macro-expander-name x)))
                     macro-expanders)))

;;
(define (expand-macro name arguments)
  (let ((filtered (filter (lambda (x)
                            (equal? name (macro-expander-name x)))
                          macro-expanders)))
    (if (empty? filtered)
        arguments
        ((macro-expander-macro (first filtered)) arguments))))

;; parse-define
(define (preprocess-define-macro s-exp)
  (if (or (not (list? s-exp))
          (empty? s-exp)
          (not (equal? 'define-macro (first s-exp))))
      #false
      (let ((stx (rest s-exp)))
        ; at least 1 argument
        (arity-match 2 (length stx))
        ; get expressions
        (when (and (not (list? (first stx)))
                   (empty? (first stx)))
          (error "<define-macro> is not match the pattern."))
        (let ((body (second stx))
              (name (first (first stx)))
              (arguments (rest (first stx))))
          (cond
            ;[(not (and (list? body)
            ;           (member (first body) '(quote quasiquote))))
            ; (error "<body> of macro must be a <quoted> or a <quasiquoted>.")]
            ;; checking name must be a symbol and not a reversed words
            [(not (parse-symbol name))
             (error "<name> of macro must be <symbol>.")]
            [else
             ;; create and install macro expander
             (install-macro-expander name
                                     (lambda (args)
                                       (arity-match (length args) (length arguments))
                                       (define (expand exp)
                                         (cond
                                           [(list? exp)
                                            (map expand exp)]
                                           [(symbol? exp)
                                            (let ((member? (member exp arguments)))
                                              (if member?
                                                  (list-ref args (- (length arguments)
                                                                    (length member?)))
                                                  exp))]
                                           [else exp]))
                                       (expand body)))])))))

(define (preprocess-include s-exp)
  (let ((files (include-stmt-files (parse-include s-exp))))
    `(,(first s-exp)
      ,@(reverse
         (let loop ((a-list files)
                    (result null))
           (if (empty? a-list)
               result
               (loop (rest a-list)
                     (cond
                       [(equal? 'macro (pair-expr-right (first a-list)))
                        (unless (file-exists? (atom-expr-data (pair-expr-left (first a-list))))
                          (error "file not exists"))
                        (with-input-from-file (atom-expr-data (pair-expr-left (first a-list)))
                          (lambda ()
                            (let loop ((sexp (read)))
                              (unless (eof-object? sexp)
                                (preprocess sexp)
                                (loop (read))))))
                        result]
                       [else
                        (cons (atom-expr-data (pair-expr-left (first a-list)))
                              (if (pair-expr-right (first a-list))
                                  (cons '#:lib result)
                                  result))]))))))))

;; preproccess
(define (preprocess s-exp)
  (cond
    [(list? s-exp)
     (if (empty? s-exp)
         (error "empty expression.")
         (let ((the-first (first s-exp)))
           (cond
             [(equal? the-first 'define-macro)
              (preprocess-define-macro s-exp)
              (void)]
             [(equal? the-first '%include)
              (preprocess-include s-exp)]
             [(has-macro-expander? the-first)
              (expand-macro the-first (rest s-exp))]
             [else (filter (lambda (x) (not (void? x)))
                           (map preprocess s-exp))])))]
    [(symbol? s-exp)
     (when (has-macro-expander? s-exp)
       (error (string-append "bad syntax with macro: "
                             (symbol->string s-exp))))
     s-exp]
    [else s-exp]))