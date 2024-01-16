#lang racket

;; Ceta programming languages by MaiHD

(provide (all-defined-out))

;; ast-node
(struct ast-node () #:transparent)
;; expr : struct
(struct expr ast-node () #:transparent)

;; atom-expr : (struct any/c)
(struct atom-expr expr (data) #:transparent)
(struct int-expr atom-expr () #:transparent)
(struct bool-expr atom-expr () #:transparent)
(struct char-expr atom-expr () #:transparent)
(struct long-atom atom-expr () #:transparent)
(struct float-expr atom-expr () #:transparent)
(struct short-atom atom-expr () #:transparent)
(struct double-atom atom-expr () #:transparent)
(struct string-expr atom-expr () #:transparent)
(struct symbol-expr atom-expr () #:transparent)
(struct keyword-expr atom-expr () #:transparent)

;;
(struct s-expr expr () #:transparent)
(struct raw-expr s-expr (data) #:transparent)
(struct call-expr s-expr (caller arguments) #:transparent)
(struct cast-expr s-expr (type target) #:transparent)
(struct unary-expr s-expr (operator operand) #:transparent)
(struct binary-expr s-expr (operator operands) #:transparent)
(struct trinary-expr s-expr (condition left right) #:transparent)
(struct return-expr s-expr (return) #:transparent)

(struct access-expr s-expr (operator target members) #:transparent)
(struct index-expr s-expr (target indexes) #:transparent)

;;
;; statement structs
(struct stmt s-expr () #:transparent)

;;
;; preprocessor statement structs
(struct prep-stmt stmt () #:transparent)
(struct include-stmt prep-stmt (files) #:transparent)
(struct defalias-stmt prep-stmt (name alias) #:transparent)
(struct defmacro-stmt prep-stmt (name arguments body) #:transparent)
(struct undef-stmt prep-stmt (target) #:transparent)
(struct ifprep-stmt prep-stmt (clauses) #:transparent)
(struct defined-stmt prep-stmt (target) #:transparent)
(struct errprep-stmt prep-stmt (message) #:transparent)
(struct line-stmt prep-stmt (line filename) #:transparent)
(struct pragma-stmt prep-stmt (sequence) #:transparent)
(struct newline-stmt prep-stmt () #:transparent)

;;
;; definition statement structs
(struct def-stmt stmt (name classes) #:transparent)
(struct defun-stmt def-stmt (return arguments body) #:transparent)
(struct defvar-stmt def-stmt (type definition) #:transparent)

;;
;; declaration statement structs
(struct decl-stmt stmt (name classes) #:transparent)
(struct declfun-stmt decl-stmt (return arguments) #:transparent)
(struct declvar-stmt decl-stmt (type) #:transparent)

;;
;; program structs
(struct progn-stmt stmt (body) #:transparent)


(struct if-expr s-expr (clauses) #:transparent)
(struct switch-expr s-expr (target clauses) #:transparent)
(struct condition-claus-expr s-expr (else? condition body) #:transparent)

(struct for-expr s-expr (symbol begin final condition body) #:transparent)
(struct while-expr s-expr (symbol condition body) #:transparent)

(struct goto-expr s-expr (label) #:transparent)
(struct break-expr s-expr () #:transparent)
(struct label-expr s-expr (label) #:transparent)
(struct continue-expr s-expr () #:transparent)

(struct attribute-expr s-expr (attribute definition) #:transparent)

(struct arr-expr s-expr (type size) #:transparent)
(struct ptr-expr s-expr (type) #:transparent)
(struct fptr-expr s-expr (return arguments) #:transparent)

(struct pair-expr s-expr (left right) #:transparent)
(struct tuple-expr s-expr (items) #:transparent)

;;
;; type declare and definition
(struct type-stmt s-expr (name definition) #:transparent)
(struct enum-stmt type-stmt () #:transparent)
(struct struct-stmt type-stmt (symbol) #:transparent)
(struct deftype-stmt type-stmt () #:transparent)

;;
;; current-ast: parameter
(define current-ast (make-parameter #false))