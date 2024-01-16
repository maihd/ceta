#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ceta programming languages by MaiHD
;;; 
;;; Main syntax parser function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide parse parse-symbol parse-include define-parser force-parse make-force-parse compose-parser)

(require "utils.rkt" "ast.rkt")

(define parsing-stack (stack))
(define (begin-parse expr-type)
  (set! parsing-stack (stack-push parsing-stack expr-type)))
(define (end-parse)
  (set! parsing-stack (stack-pop parsing-stack)))

;; force-parse : procedure? s-expression? -> any/c
(define (force-parse a-parser s-exp)
  (let ([parsed (a-parser s-exp)])
    (if parsed
        parsed
        (error "error. "))))

;; make-force-parse : procdeure? -> procedure?
(define (make-force-parse a-parser)
  (lambda (s-exp)
    (force-parse a-parser s-exp)))

;; compose-parser
(define (compose-parser . parsers)
  ; loop : (listof procedure?) s-expression? -> (or/c expr? #false)
  (define (loop a-list s-exp parsed)
    (if (or parsed (empty? a-list))
        parsed
        (loop (rest a-list)
              s-exp
              ((first a-list) s-exp))))
  ; the composed parser
  (lambda (s-exp)
    (loop parsers s-exp #false)))

;; make-pair-parser : procedure? proedure? -> procedure?
(define (make-pair-parser left-parser right-parser)
  (lambda (s-exp)
    (if (not (list? s-exp))
        #false
        (begin
          (arity-match 2 (length s-exp))
          (pair-expr (force-parse left-parser (first s-exp))
                     (force-parse right-parser (second s-exp)))))))
;; parse-pair :
(define (parse-pair s-exp left-parser right-parser)
  ((make-pair-parser left-parser right-parser) s-exp))

;; make-tuple-parser : (listof procedure?) -> procedure?
(define (make-tuple-parser . parsers)
  (lambda (s-exp)
    ; checking arity
    (when (list? s-exp)
      (arity-match (length parsers) (length s-exp)))
    ; begin parse
    (if (not (list? s-exp))
        #false
        (tuple-expr
         (reverse
          (let loop ([parsed null]
                     [a-list parsers]
                     [p-list s-exp])
           (if (empty? a-list)
               (if (empty? parsed)
                   #false
                   parsed)
               (loop (cons (force-parse (first a-list)
                                        (first p-list))
                           parsed)
                     (rest a-list)
                     (rest p-list)))))))))
;; (parse-tuple :
(define (parse-tuple s-exp . parsers)
  ((apply make-tuple-parser parsers) s-exp))

;; multi-assert
(define (member-assert given . a-list)
  (assert (member given a-list)
          "expecting."))

;; parser-binding
(struct parser-binding (symbols parser))
;; parser-table
(define parser-table (list))
;; parser-table-add
(define (parser-table-add symbols a-parser)
  (set! parser-table
        (cons (parser-binding symbols a-parser)
              parser-table)))
;; parser-table-lookup
(define (parser-table-lookup a-symbol)
  (define found
    (for/list ([binding parser-table]
        #:when (member a-symbol (parser-binding-symbols binding)))
    (parser-binding-parser binding)))
  (if (empty? found)
      #false
      (first found)))
;; parser-symbols-table : (listof symbol?)
(define parser-symbols-table (list))
;; parser-special-symbol? : symbol? -> boolean?
(define (parser-special-symbol? a-symbol)
  (if (member a-symbol parser-symbols-table)
      #true
      #false))

;; define-parser : syntax
(define-syntax define-parser
  (syntax-rules (type symbol parse make)
    [(_ name (literial-symbols ...)
        (type expr-type)
        (symbol symbol-value ...)
        (parse (symbol-id argument) body ...)
        (make a-expr ...))
     (define name
       (let* ([symbols-table (list symbol-value ...)]
              [the-parser
               ; generate parser lambda
               (lambda (argument)
                 (cond
                   [(not (list? argument))
                    #false]
                   [(and (not (empty? symbols-table))
                         (not (member (first argument) symbols-table)))
                    #false]
                   [else
                    (define symbol-id (first argument))
                    (begin-parse expr-type)
                    ;(when (not (empty? (list symbol-value ...))) 
                    ;  (member-assert (first argument) symbol-value ...))
                    (set! argument (rest argument))
                    body
                    ...
                    (end-parse)
                    (expr-type a-expr ...)]))])
         ; add special symbols
         (set! parser-symbols-table
               (append parser-symbols-table
                       (list literial-symbols ...)
                       (list symbol-value ...)))
         ; add parser to table
         (parser-table-add (list symbol-value ...) the-parser)
         ; return the parser
         the-parser))
     ]
    [(_ name (literial-symbols ...)
        (type expr-type)
        (symbol symbol-value ...)
        (make a-expr ...)
        (define (symbol-id argument) body ...))
     (define name
       (let* ([symbols-table (list symbol-value ...)]
              [the-parser
               ; generate parser lambda
               (lambda (argument)
                 (cond
                   [(not (list? argument))
                    #false]
                   [(and (not (empty? symbols-table))
                         (not (member (first argument) symbols-table)))
                    #false]
                   [else
                    (define symbol-id (first argument))
                    (begin-parse expr-type)
                    ;(when (not (empty? (list symbol-value ...))) 
                    ;  (member-assert (first argument) symbol-value ...))
                    (set! argument (rest argument))
                    body
                    ...
                    (end-parse)
                    (expr-type a-expr ...)]))])
         ; add special symbols
         (set! parser-symbols-table
               (append parser-symbols-table
                       (list literial-symbols ...)
                       (list symbol-value ...)))
         ; add parser to table
         (parser-table-add (list symbol-value ...) the-parser)
         ; return the parser
         the-parser))
     ]))

;; parse : s-expression? -> string?
(define (parse s-exp)
  (or (parse-s-expr s-exp)
      (parse-atom s-exp)))

;; parse-s-expr : s-expression? -> (or/c s-expr? #false)
(define (parse-s-expr s-exp)
  (if (not (list? s-exp))
      #false
      (cond [(empty? s-exp)
             (error "empty expression.")]
            [else
             (let ([parser (parser-table-lookup (first s-exp))])
               (if parser
                   (parser s-exp)
                   (parse-call s-exp)))])))

;; parse-atom : s-expression? -> (or/c atom-expr? #false)
(define (parse-atom s-exp)
  (for/first ([the-parser atom-parser-table]
              #:when (the-parser s-exp))
    (the-parser s-exp)))

;; atom-parser-table : (listof procedure?)
(define atom-parser-table (list))
;; define-atom-parser : (or/c struct? procedure?) procedure? -> procedure?
(define-syntax-rule (define-atom-parser name atom-type pred)
  (define name
    (let ([the-parser
           ; generate parser
           (lambda (s-exp)
             (if (pred s-exp)
                 (atom-type s-exp)
                 #false))])
      ; add to table
      (set! atom-parser-table (cons the-parser atom-parser-table))
      ; set to global
      the-parser)))
;;
(define-atom-parser parse-char char-expr char?)
(define-atom-parser parse-bool bool-expr boolean?)
(define-atom-parser parse-float float-expr double-flonum?)
(define-atom-parser parse-int int-expr exact-integer?)
(define-atom-parser parse-string string-expr string?)
(define-atom-parser parse-symbol symbol-expr
  (lambda (s-exp)
    (if (symbol? s-exp)
        (cond
          [(or (parse-premitive s-exp)
               (parser-special-symbol? s-exp))
           (error (string-append "bad syntax: "
                                 (symbol->string s-exp)))]
          [(equal? "c:" (symbol->string s-exp))
           (error "empty <c-symbol>.")]
          [else #true])
        #false)))
(define-atom-parser parse-atom-keyword keyword-expr
  (lambda (s-exp)
    (if (keyword? s-exp)
        (error "<keyword> is not used in <atom> way.")
        #false)))
(define-atom-parser parse-premitive
  (lambda (s-exp)
    (case s-exp
      [(int-ptr char-ptr void-ptr float-ptr double-ptr short-ptr long-ptr unsigned-ptr)
       (ptr-expr (symbol-expr
                  (string->symbol
                   (first
                    (string-split
                     (symbol->string s-exp) "-")))))]
      [(int-array char-array float-array double-array short-array long-array unsinged-array)
       (arr-expr (symbol-expr
                  (string->symbol
                   (first
                    (string-split
                     (symbol->string s-exp) "-")))))]
      [else (symbol-expr s-exp)]))
  (lambda (s-exp)
    (if (member s-exp '(int
                        char void float double short long unsigned
                        int-ptr char-ptr void-ptr float-ptr double-ptr short-ptr long-ptr unsigned-ptr
                        int-array char-array float-array double-array short-array long-array unsinged-array))
        #true
        #false)))

(define (parse-keyword s-exp)
  (if (keyword? s-exp)
      (keyword-expr s-exp)
      #false))

(define (parse-reversed-word s-exp)
  (if (parser-special-symbol? s-exp)
      (symbol-expr s-exp)
      #false))

;; parse-include
(define-parser parse-include ()
  (type include-stmt)
  (symbol 'include)
  (parse (the-symbol s-exp)
         (define (loop a-list result)
           (if (empty? a-list)
               result
               (let ((current (first a-list)))
                 (let-values
                     (((parsed rest-list)
                       (if (keyword? current)
                           (case current
                             [(#:clib)
                              (values (pair-expr (force-parse (compose-parser parse-string
                                                                              parse-symbol)
                                                              (second a-list))
                                                  'library)
                                      (cddr a-list))]
                             [(#:macro)
                              (values (pair-expr (force-parse (compose-parser parse-string
                                                                              parse-symbol)
                                                              (second a-list))
                                                  'macro)
                                      (cddr a-list))]
                             [else
                              (error (string-append "keyword not supported: #:" (keyword->string current)))])
                           (values (pair-expr (force-parse (compose-parser parse-string
                                                                           parse-symbol)
                                                           current)
                                              #false)
                                   (rest a-list)))))
                   (loop rest-list (cons parsed result)))))))
  (make (reverse (loop s-exp null))))

;; parse-ifprep : s-expression? -> ifprep-stmt?
(define-parser parse-ifprep ('else)
  (type ifprep-stmt)
  (symbol '%if)
  (parse (the-symbol s-exp)
         (define (parse-claus claus else?)
           (when else? (error "<else-clause> must be last."))
           (if (or (not (list? claus))
                   (empty? claus))
               (error "syntax error. <if-clause> is not well-form.")
               (if (equal? (first claus) 'else)
                   (values (condition-claus-expr #true
                                                 #false
                                                 (map parse (rest claus)))
                           #true)
                   (values (condition-claus-expr #false
                                                 (parse (first claus))
                                                 (map parse (rest claus)))
                           #false))))
         (define (loop a-list else? result)
           (if (empty? a-list)
               result
               (let-values ([(claus else-claus?)
                             (parse-claus (first a-list) else?)])
                 (loop (rest a-list)
                       else-claus?
                       (cons claus result))))))
  (make
   (reverse (loop s-exp #false (list)))))

;; parse-ifprep : s-expression? -> ifprep-stmt?
(define-parser parse-ifdef ('else)
  (type ifprep-stmt)
  (symbol '%ifdef)
  (parse (the-symbol s-exp)
         (arity-min-match 2 (length s-exp)))
  (make (list
         (condition-claus-expr #false
                               (parse (list '%define? (first s-exp)))
                               (map parse (rest s-exp))))))

;; parse-undef : s-expression? -> undef-stmt?
(define-parser parse-undef ()
  (type undef-stmt)
  (symbol '%undef)
  (parse (the-symbol s-exp)
         (arity-match 1 (length s-exp)))
  (make (force-parse parse-symbol (first s-exp))))

;; parse-defined : s-expression? -> ifprep-stmt?
(define-parser parse-defined ()
  (type defined-stmt)
  (symbol 'define? 'defined)
  (parse (the-symbol s-exp)
         (arity-match 1 (length s-exp)))
  (make (force-parse parse-symbol (first s-exp))))

;; parse-defined : s-expression? -> ifprep-stmt?
(define-parser parse-errprep ()
  (type errprep-stmt)
  (symbol '%error)
  (parse (the-symbol s-exp)
         (arity-match 1 (length s-exp)))
  (make (force-parse parse-string (first s-exp))))

;; parse-defined : s-expression? -> ifprep-stmt?
(define-parser parse-line ()
  (type line-stmt)
  (symbol '%line)
  (parse (the-symbol s-exp)
         (arity-min-match 1 (length s-exp)))
  (make (force-parse parse-atom (first s-exp))
        (if (empty? (rest s-exp))
            #false
            (force-parse parse-string (second s-exp)))))

;; parse-defined : s-expression? -> ifprep-stmt?
(define-parser parse-pragma ()
  (type errprep-stmt)
  (symbol '%pragma)
  (parse (the-symbol s-exp))
  (make (map parse s-exp)))

;; parse-defined : s-expression? -> ifprep-stmt?
(define-parser parse-newline ()
  (type newline-stmt)
  (symbol '%newline '%backslash)
  (parse (the-symbol s-exp)
         (arity-match 0 (length s-exp)))
  (make))

;;
(define-parser parse-define-alias ()
  (type defalias-stmt)
  (symbol '%define-alias)
  (parse (the-symbol s-exp)
         (arity-min-match 1 (length s-exp)))
  (make (force-parse parse-symbol (first s-exp))
        (map parse (rest s-exp))))

;;
(define-parser parse-define-macro ()
  (type defmacro-stmt)
  (symbol '%define-c-macro)
  (parse (the-symbol s-exp)
         (arity-min-match 1 (length s-exp))
         (define-values (name arguments macro-body)
           (let ((the-first (first s-exp))
                 (body (rest s-exp)))
             (cond
               [(list? the-first)
                (arity-min-match 2 (length the-first))
                  (values (force-parse parse-symbol (first the-first))
                          (map (make-force-parse parse-symbol parse-type)
                               (rest the-first))
                          (map parse body))]

               [else
                (error "unexpected in <define-c-macro>.")]))))
  (make name arguments macro-body))

;; parse-ifprep : s-expression? -> ifprep-stmt?
(define-parser parse-ifndef ()
  (type ifprep-stmt)
  (symbol '%ifndef)
  (parse (the-symbol s-exp)
         (arity-min-match 2 (length s-exp)))
  (make (list
         (condition-claus-expr #false
                               (unary-expr '! (parse (list '%define? (first s-exp))))
                               (map parse (rest s-exp))))))

;; parse-access : s-expression? -> access-expr?
(define-parser parse-access ()
  (type access-expr)
  (symbol '-> '::)
  (parse (the-operator s-exp)
         (arity-min-match 2 (length s-exp))) 
  (make the-operator
        (force-parse
         (compose-parser parse-symbol
                         parse-s-expr)
         (first s-exp))
        (map (make-force-parse parse-symbol)
             (rest s-exp))))

;; parse-index : s-expression? -> access-expr?
(define-parser parse-index ()
  (type index-expr)
  (symbol 'aref)
  (parse (the-operator s-exp)
         (arity-min-match 2 (length s-exp))) 
  (make (force-parse
         (compose-parser parse-symbol
                         parse-s-expr)
         (first s-exp))
        (map (make-force-parse parse)
             (rest s-exp))))

;; parse-progn
(define-parser parse-progn ()
  (type progn-stmt)
  (symbol 'progn)
  (parse (the-symbol s-exp))
  (make (map parse s-exp)))

;; parse-if : s-expression? -> if-expr?
(define-parser parse-if ('else)
  (type if-expr)
  (symbol 'if)
  (parse (the-symbol s-exp)
         (define (parse-claus claus else?)
           (when else? (error "<default-claus> must be last."))
           (if (or (not (list? claus))
                   (empty? claus))
               (error "syntax error. <if-claus> is not well-form.")
               (if (equal? (first claus) 'else)
                   (values (condition-claus-expr #true
                                                 #false
                                                 (map parse (rest claus)))
                           #true)
                   (values (condition-claus-expr #false
                                                 (parse (first claus))
                                                 (map parse (rest claus)))
                           #false))))
         (define (loop a-list else? result)
           (if (empty? a-list)
               result
               (let-values ([(claus else-claus?)
                             (parse-claus (first a-list) else?)])
                 (loop (rest a-list)
                       else-claus?
                       (cons claus result))))))
  (make
   (reverse (loop s-exp #false (list)))))

;; parse-case : s-expression? -> case-expr?
(define-parser parse-case ()
  (type if-expr)
  (symbol 'case)
  (parse (the-symbol s-exp)
         (arity-min-match 1 (length s-exp))
         (define the-target (parse (first s-exp)))
         (define (parse-claus claus else?)
           (when else? (error "<else-claus> must be last."))
           (if (or (not (list? claus))
                   (empty? claus))
               (error "syntax error. <case-claus> is not well-form.")
               (if (equal? (first claus) 'else)
                   (values (condition-claus-expr #true
                                                 #false
                                                 (map parse (rest claus)))
                           #true)
                   (values (condition-claus-expr #false
                                                 (binary-expr '==
                                                              (list the-target
                                                                        (parse (first claus))))
                                                 (map parse (rest claus)))
                           #false))))
         (define (loop a-list else? result)
           (if (empty? a-list)
               result
               (let-values ([(claus else-claus?)
                             (parse-claus (first a-list) else?)])
                 (loop (rest a-list)
                       else-claus?
                       (cons claus result))))))
  (make
   (reverse (loop (rest s-exp)
                  #false
                  (list)))))

;; parse-switch : s-expression? -> case-expr?
(define-parser parse-switch ('default)
  (type switch-expr)
  (symbol 'switch)
  (parse (the-symbol s-exp)
         (arity-min-match 1 (length s-exp))
         (define (parse-claus claus else?)
           (when else? (error "<default-claus> must be last."))
           (if (or (not (list? claus))
                   (empty? claus))
               (error "syntax error. <switch-claus> is not well-form.")
               (if (equal? (first claus) 'default)
                   (values (condition-claus-expr #true
                                                 #false
                                                 (map parse (rest claus)))
                           #true)
                   (values (condition-claus-expr #false
                                                 (force-parse parse-atom (first claus))
                                                 (map parse (rest claus)))
                           #false))))
         (define (loop a-list else? result)
           (if (empty? a-list)
               result
               (let-values ([(claus else-claus?)
                             (parse-claus (first a-list) else?)])
                 (loop (rest a-list)
                       else-claus?
                       (cons claus result))))))
  (make
   (parse (first s-exp))
   (reverse (loop (rest s-exp)
                  #false
                  (list)))))

;; parse-for
(define-parser parse-for ()
  (type for-expr)
  (symbol 'for)
  (parse (the-symbol s-exp)
         (arity-min-match 3 (length s-exp))
         (define (parse-begin s-exp)
           (when (not (list? s-exp))
             (error "not bounded <for-begin>."))
           (map parse s-exp))
         (define (parse-final s-exp)
           (when (not (list? s-exp))
             (error "not bounded <for-final>."))
           (map parse s-exp))
         (define (parse-condition s-exp)
           (force-parse (compose-parser parse-unary
                                        parse-binary
                                        parse-access
                                        parse-index
                                        parse-call
                                        parse-atom)
                        s-exp)))
  (make the-symbol
        (parse-begin (first s-exp))
        (parse-final (second s-exp))
        (parse-condition (third s-exp))
        (map parse (cdddr s-exp))))

;; parse-while
(define-parser parse-while ()
  (type while-expr)
  (symbol 'while 'do-while)
  (parse (the-symbol s-exp)
         (arity-min-match 1 (length s-exp)))
  (make the-symbol
        (force-parse (compose-parser parse-unary
                                     parse-binary
                                     parse-access
                                     parse-index
                                     parse-call
                                     parse-atom)
                     (first s-exp))
        (map parse (rest s-exp))))

;; parse-call
(define-parser parse-call ()
  (type call-expr)
  (symbol)
  (parse (caller s-exp))
  (make (force-parse parse caller)
        (map parse s-exp)))

;;
(define (parse-class s-exp)
  (cond
    [(symbol? s-exp)
     ;(and (symbol? s-exp)
     ;     (member s-exp '(static inline extern auto register volatile __stdcall)))
     (symbol-expr s-exp)]
    [(and (list? s-exp)
          (= (length s-exp) 2)
          (member (first s-exp) '(__declspec)))
     (parse-call s-exp)]
    [else #false]))

;;
(define (parse-classes s-exp)
  (define the-parser (make-force-parse parse-class))
  (let loop ((a-list s-exp)
             (result null))
    (if (or (empty? a-list)
            (not (keyword? (first a-list))))
        result
        (loop (rest (rest a-list))
              (if (equal? '#:class (first a-list))
                  (cons (the-parser (second a-list)) result)
                  (error "not supported keyword."))))))

;; parse-define
(define-parser parse-define ()
  (type (lambda (x) x))
  (symbol 'define)
  (parse (the-symbol s-exp)
         ; at least 1 argument
         (arity-min-match 1 (length s-exp))
         ; get expressions
         (define the-first (first s-exp))
         (define body (rest s-exp))
         ; begin parse
         (define parsed
           (cond
             [(symbol? the-first)
              (let ((classes (parse-classes (rest body))))
                (defvar-stmt
                  (force-parse parse-symbol the-first)
                  classes
                  (force-parse parse-type (first body))
                  (force-parse parse-calc-expr
                               (first
                                (list-tail (rest body)
                                           (* 2 (length classes)))))))]
             [(list? the-first)
              (arity-min-match 2 (length the-first))
              (let ((classes (parse-classes body)))
                (defun-stmt
                  (force-parse parse-symbol (first the-first))
                  classes
                  (force-parse parse-type (second the-first))
                  (map (make-force-parse (compose-parser parse-type
                                                         (make-pair-parser parse-symbol parse-type)))
                       (rest (rest the-first)))
                  (map parse (list-tail body (* 2 (length classes))))))]

             [else
              (error "unexpected in <define>.")])))
  (make parsed))

;; parse-defn
(define-parser parse-defn ()
  (type (lambda (x) x))
  (symbol 'defn)
  (make parsed)
  (define (the-symbol s-exp)
    ;; at least 1 argument
    (arity-min-match 1 (length s-exp))
    
    ;; get expressions
    (define the-first (first s-exp))
    (define body (rest s-exp))
    
    (define (iter name a-list args)
    (let ((current (first a-list)))
      (cond
        ((equal? '-> current)
         (defun-stmt
           name
           '()
           (force-parse parse-type (second a-list))
           (reverse args)
           (map parse (rest (rest a-list)))))
        ((or (not (list? current))
             (not (= 3 (length current)))
             (not (symbol? (first current)))
             (not (symbol? (third current)))
             (not (equal? ': (second current))))
         (error "<defn>: function signature is wrong!"))
        (else
         (iter name
               (rest a-list)
               (cons (pair-expr (force-parse parse-symbol (first current))
                                (force-parse parse-type (third current)))
                     args))))))
    
    ;; begin parse
    (define parsed
      (cond
        [(symbol? the-first)
         (let ((name (force-parse parse-symbol the-first)))
           (iter name body '()))]
        
        [else
         (error "#<defn> is bad syntax.")]))))

;; parse-declare
(define-parser parse-declare ()
  (type (lambda (x) x))
  (symbol 'declare)
  (parse (the-symbol s-exp)
         ; at least 1 argument
         (arity-min-match 1 (length s-exp))
         ; get expressions
         (define the-first (first s-exp))
         (define body (rest s-exp))
         ; begin parse
         (define parsed
           (cond
             [(symbol? the-first)
              (declvar-stmt
                (force-parse parse-symbol the-first)
                (let ((classes (parse-classes (rest body))))
                  (when (not (= (* 2 (length classes))
                                (length (rest body))))
                    (error "unexpected in <declare-variable>."))
                  classes)
                (force-parse parse-type (first body)))]
             [(list? the-first)
              (arity-min-match 2 (length the-first))
              (declfun-stmt
                (force-parse parse-symbol (first the-first))
                (let ((classes (parse-classes body)))
                  (when (not (= (* 2 (length classes))
                                (length body)))
                    (error "unexpected in <declare-function>."))
                  classes)
                (force-parse parse-type (second the-first))
                (map (make-force-parse (compose-parser parse-type
                                                       (make-pair-parser parse-symbol parse-type)))
                     (rest (rest the-first))))]

             [else
              (error "unexpected in <define>.")])))
  (make parsed))

;;
(define-parser parse-array ()
  (type arr-expr)
  (symbol 'array)
  (parse (the-operator s-exp)
         (arity-min-match 1 (length s-exp))
         (arity-max-match 2 (length s-exp)))
  (make (force-parse parse-type (first s-exp))
        (if (< (length s-exp) 2)
            #false
            (force-parse parse-atom (second s-exp)))))

;;
(define-parser parse-ptr ()
  (type ptr-expr)
  (symbol 'ptr)
  (parse (the-operator s-exp)
         (arity-match 1 (length s-exp)))
  (make (force-parse parse-type (first s-exp))))

;;
(define-parser parse-return ()
  (type return-expr)
  (symbol 'return)
  (parse (the-operator s-exp))
  (make (if (empty? s-exp)
            #false
            (force-parse parse-calc-expr (first s-exp)))))


;; parse-unary
(define-parser parse-unary ()
  (type unary-expr)
  (symbol '! '++ '-- '1++ '1--
          ;'not
          'ref 'deref)
  (parse (the-operator s-exp)
         (arity-match 1 (length s-exp)))
  (make the-operator
        (force-parse parse-calc-expr
                     (first s-exp))))

;; parse-sizeof
(define-parser parse-sizeof ()
  (type call-expr)
  (symbol 'sizeof)
  (parse (the-symbol s-exp)
         (arity-match 1 (length s-exp)))
  (make (symbol-expr the-symbol)
        (list (force-parse (compose-parser parse-atom
                                           parse-type)
                           (first s-exp)))))

(define-parser parse-cast ()
  (type cast-expr)
  (symbol 'as)
  (parse (the-symbol s-exp)
         (arity-match 2 (length s-exp)))
  (make (force-parse parse-type (first s-exp))
        (force-parse parse (second s-exp))))

;;
(define-parser parse-goto ()
  (type goto-expr)
  (symbol 'goto)
  (parse (the-symbol s-exp)
         (arity-match 1 (length s-exp)))
  (make (force-parse parse-keyword (first s-exp))))

;;
(define-parser parse-break ()
  (type break-expr)
  (symbol 'break)
  (parse (the-symbol s-exp)
         (arity-match 0 (length s-exp)))
  (make))

;;
(define-parser parse-label ()
  (type label-expr)
  (symbol ':)
  (parse (the-symbol s-exp)
         (arity-match 1 (length s-exp)))
  (make (force-parse parse-keyword (first s-exp))))

;;
(define-parser parse-continue ()
  (type continue-expr)
  (symbol 'continue)
  (parse (the-symbol s-exp)
         (arity-match 0 (length s-exp)))
  (make))

;; parse-binary
(define-parser parse-binary ()
  (type binary-expr)
  (symbol '+ '- '* '/ '%
          '<< '>> '& '^ '\|
          '&& '||
          '== '!= '< '> '<= '>=
          '\|= '^= '&=
          '<<= '>>=)
  (parse (the-operator s-exp)
         (arity-min-match 2 (length s-exp)))
  (make the-operator
        (map (make-force-parse parse-calc-expr)
              s-exp)))

;; parse-trinary
(define-parser parse-trinary ()
  (type trinary-expr)
  (symbol '?)
  (parse (the-operator s-exp)
         (arity-min-match 3 (length s-exp)))
  (make (force-parse parse-calc-expr (first s-exp))
        (force-parse parse-calc-expr (second s-exp))
        (force-parse parse-calc-expr (third s-exp))))

;;
;; parse-assign
(define-parser parse-assign ()
  (type binary-expr)
  (symbol '= '+= '-= '*= '/= '%=)
  (parse (the-operator s-exp)
         (arity-min-match 2 (length s-exp)))
  (make the-operator
        (flatten `(,(map
                     (make-force-parse
                      (compose-parser parse-access
                                      parse-unary
                                      parse-index
                                      parse-cast
                                      parse-symbol))
                     (take s-exp (- (length s-exp) 1)))
                   ,(force-parse parse-calc-expr (last s-exp))))))

;; parse-fptr
(define-parser parse-fptr ()
  (type fptr-expr)
  (symbol 'fptr)
  (parse (the-symbol s-exp)
         (arity-min-match 1 (length s-exp)))
  (make (force-parse parse-type (first s-exp))
        (if (empty? (rest s-exp))
            null
            (map (make-force-parse (compose-parser parse-type
                                                   (make-pair-parser parse-symbol parse-type)))
                 (rest s-exp)))))

;; parse-enum
(define-parser parse-enum ()
  (type enum-stmt)
  (symbol 'enum)
  (parse (the-symbol s-exp)
         (arity-min-match 1 (length s-exp))
         (define the-name (parse-symbol (first s-exp)))
         (when the-name
           (set! s-exp (rest s-exp)))
         (define definition
           (if (empty? s-exp)
               #false
               (map (make-force-parse
                     (compose-parser parse-symbol
                                     (make-pair-parser parse-symbol parse-atom)))
                    s-exp))))
  (make the-name definition))

;; parse field
(define (parse-field s-exp)
  ;; define the parser base on length of s-exp
  (define the-parser
    (case (length s-exp)
    [(2)
     (make-tuple-parser parse-symbol parse-type)]
    [(4)
     (make-tuple-parser parse-symbol
                        parse-type
                        (lambda (e)
                          (if (equal? e '#:bit)
                              (symbol-expr ':)
                              #false))
                        parse-atom)] 
    [else
     (error
      (string-append "arity mismatch in <field>. expecting: 2 or 4. given: "
                     (number->string (length s-exp))))]))
  
  ;; call the parser
  (the-parser s-exp))

;; parse-struct
(define-parser parse-struct ()
  (type struct-stmt)
  (symbol 'union 'struct)
  (parse (the-symbol s-exp)
         (arity-min-match 1 (length s-exp))
         (define the-name (parse-symbol (first s-exp)))
         (define definition
           (let ((body
                  (if the-name
                      (rest s-exp)
                      s-exp)))
             (if (empty? body)
                 #false
                 (map (make-force-parse parse-field) body)))))
  (make the-name definition the-symbol))

;; parse-deftype
(define-parser parse-deftype ()
  (type deftype-stmt)
  (symbol 'deftype)
  (parse (the-symbol s-exp)
         (arity-match 2 (length s-exp)))
  (make (force-parse parse-symbol (first s-exp))
        (force-parse parse-type (second s-exp))))

;;
(define-parser parse-attribute ()
  (type attribute-expr)
  (symbol 'inline 'extern 'auto 'static  'register 'const)
  (parse (the-symbol s-exp)
         (arity-match 1 (length s-exp)))
  (make the-symbol (parse (first s-exp))))


;; parse-type
(define (parse-type s-exp)
  (define s-parser 
    (compose-parser parse-premitive
                    parse-attribute
                    parse-array
                    parse-ptr
                    parse-enum
                    parse-fptr
                    parse-struct))
  (cond
    [(parse-premitive s-exp) (parse-premitive s-exp)]
    [(parse-symbol s-exp) (parse-symbol s-exp)]
    [(parse-atom s-exp) #false]
    [else (s-parser s-exp)]))

;;
(define parse-calc-expr
  (compose-parser parse-atom
                  parse-unary
                  parse-access
                  parse-sizeof
                  parse-index
                  parse-binary
                  parse-trinary
                  parse-cast
                  parse-call))