#lang racket

;; Ceta programming languages by MaiHD

(provide generate)

(require "ast.rkt")

;;
(define (generate-arguments a-generator a-separator arguments)
  (string-join (map a-generator arguments)
               a-separator))

;;
(define (generate-argument arg-expr)
  (if (pair-expr? arg-expr)
      (generate-type (pair-expr-right arg-expr) (pair-expr-left arg-expr))
      (generate-symbol arg-expr)))

;;
(define (generate-sequence a-generator a-separator a-sequence)
  (apply string-append
         (if (empty? a-sequence)
             '("")
             (for/list ([i (in-naturals 0)]
                        [e a-sequence])
               (string-append (a-generator e)
                              a-separator)))))

;; (listof generator?)
(define generator-table (list))
;; generator-table-add
(define (generator-table-add pred a-generator)
  (set! generator-table
        (cons (cons pred a-generator)
              generator-table)))
;; generator-table-lookup
(define (generator-table-lookup expr)
  (for/first ([g generator-table]
              #:when ((car g) expr))
    (cdr g)))
;;
(define-syntax define-generator
  (syntax-rules (predicate generate)
    [(_ name
        (predicate pred)
        (define (expr-id) body ...))
     (define name
       (let ([the-generator
              (lambda (expr-id)
                body ...)])
         (generator-table-add pred the-generator)
         the-generator))]))

;; 
(define (generate expr)
  (let ([the-generator (generator-table-lookup expr)])
    (if the-generator (the-generator expr) "")))

;;
(define-generator generate-include
  (predicate include-stmt?)
  (define (stmt)
    (apply string-append
           (map (lambda (e)
                  (string-append "#include "
                                 (if (pair-expr-right e)
                                     (string-append "<" (atom-expr-data (pair-expr-left e)) ">")
                                     (generate (pair-expr-left e)))
                                 "\n"))
                (include-stmt-files stmt)))))

;;
(define-generator generate-ifprep
  (predicate ifprep-stmt?)
  (define (stmt)
    (string-append
     (apply string-append
           (for/list ([claus (ifprep-stmt-clauses stmt)]
                      [i (in-naturals 0)])
                 (string-append (if (condition-claus-expr-else? claus)
                                    "#else\n"
                                    (string-append (if (= i 0)
                                                       "#if "
                                                       "#elif ")
                                                   (generate (condition-claus-expr-condition claus))
                                                   "\n"))
                                (generate-sequence generate ";\n" (condition-claus-expr-body claus)))))
     "#endif\n")))

;;
(define-generator generate-undef
  (predicate undef-stmt?)
  (define (stmt)
    (string-append "#undef "
                   (generate (undef-stmt-target stmt))
                   "\n")))

;;
(define-generator generate-defined
  (predicate defined-stmt?)
  (define (stmt)
    (string-append "defined("
                   (generate (defined-stmt-target stmt))
                   ")")))

;;
(define-generator generate-errprep
  (predicate errprep-stmt?)
  (define (stmt)
    (string-append "#error "
                   (generate (errprep-stmt-message stmt))
                   "\n")))

;;
(define-generator generate-line
  (predicate line-stmt?)
  (define (stmt)
    (string-append "#line "
                   (generate (line-stmt-line stmt))
                   " "
                   (generate (line-stmt-filename stmt))
                   "\n")))

;;
(define-generator generate-pragma
  (predicate pragma-stmt?)
  (define (stmt)
    (string-append "#pragma "
                   (generate-sequence generate " " (pragma-stmt-sequence stmt))
                   "\n")))

;;
(define-generator generate-newline
  (predicate newline-stmt?)
  (define (stmt)
    "\\"))

;;
(define-generator generate-define-alias
  (predicate defalias-stmt?)
  (define (stmt)
    (string-append "#define "
                   (generate (defalias-stmt-name stmt))
                   " "
                   (generate-sequence generate " " (defalias-stmt-alias stmt))
                   "\n")))

;;
(define-generator generate-define-macro
  (predicate defmacro-stmt?)
  (define (stmt)
    (string-append "#define "
                   (generate (defmacro-stmt-name stmt))
                   "("
                   (generate-argument generate ", " (defmacro-stmt-arguments stmt))
                   ") "
                   (generate-sequence generate " " (defmacro-stmt-body stmt))
                   "\n")))

;;
(define-generator generate-access
  (predicate access-expr?)
  (define (expr)
    (generate-arguments generate
                        (if (equal? (access-expr-operator expr) '->)
                            "->"
                            ".")
                        (cons (access-expr-target expr) (access-expr-members expr)))))
;;
(define-generator generate-index
  (predicate index-expr?)
  (define (expr)
    (string-append (generate (index-expr-target expr))
                   "["
                   (generate-arguments generate
                                       "]["
                                       (index-expr-indexes expr))
                   "]")))

;; generate-progn
(define-generator generate-progn
  (predicate progn-stmt?)
  (define (a-stmt)
    (string-append "{\n"
                   (generate-sequence generate ";\n" (progn-stmt-body a-stmt))
                   "}")))

;; generate-if
(define-generator generate-if
  (predicate if-expr?)
  (define (expr)
    (apply string-append
           (for/list ([claus (if-expr-clauses expr)]
                      [i (in-naturals 0)])
                 (string-append (if (condition-claus-expr-else? claus)
                                    "else {\n"
                                    (string-append (if (= i 0)
                                                       "if ("
                                                       "else if (")
                                                   (generate (condition-claus-expr-condition claus))
                                                   ") {\n"))
                                (generate-sequence generate ";\n" (condition-claus-expr-body claus))
                                "} ")))))

;; generate-switch
(define-generator generate-switch
  (predicate switch-expr?)
  (define (expr)
    (string-append
           "switch ("
           (generate (switch-expr-target expr))
           ") {"
           (apply string-append
                  (for/list ([claus (switch-expr-clauses expr)]
                             [i (in-naturals 0)])
                    (string-append (if (condition-claus-expr-else? claus)
                                       "default"
                                       (generate (condition-claus-expr-condition claus)))
                                   ":\n"
                                   (generate-sequence generate ";\n" (condition-claus-expr-body claus)))))
           "}")))

;;
(define-generator generate-for
  (predicate for-expr?)
  (define (expr)
    (string-append "for ("
                   (generate-arguments generate ", " (for-expr-begin expr))
                   ";"
                   (generate (for-expr-condition expr))
                   ";"
                   (generate-arguments generate ", " (for-expr-final expr))
                   ") {\n"
                   (generate-sequence generate ";\n" (for-expr-body expr))
                   "}")))

;;
(define-generator generate-while
  (predicate while-expr?)
  (define (expr)
    (let ([condition (generate (while-expr-condition expr))])
      (if (equal? (while-expr-symbol expr) 'while)
          (string-append "while ("
                         condition
                         ") {\n"
                         (generate-sequence generate ";\n" (while-expr-body expr))
                         "}")
          (string-append "do {\n"
                         (generate-sequence generate ";\n" (while-expr-body expr))
                         "} while ("
                         condition
                         ")")))))

;;
(define-generator generate-goto
  (predicate goto-expr?)
  (define (expr)
    (string-append "goto "
                   (generate-keyword (goto-expr-label expr)))))

;;
(define-generator generate-break
  (predicate break-expr?)
  (define (expr)
    "break"))

;;
(define-generator generate-label
  (predicate label-expr?)
  (define (expr)
    (string-append (generate-keyword (label-expr-label expr))
                   ":")))

;;
(define-generator generate-continue
  (predicate continue-expr?)
  (define (expr)
    "continue"))

;; generate-defun
(define-generator generate-defun
  (predicate defun-stmt?)
  (define (stmt)
    (string-append (generate-sequence generate " " (def-stmt-classes stmt))
                   (generate-type (defun-stmt-return stmt)
                                  (def-stmt-name stmt))
                   "("
                   (generate-arguments generate-argument ", " (defun-stmt-arguments stmt))
                   ") {\n"
                   (generate-sequence generate ";\n" (defun-stmt-body stmt))
                   "}")))

;; generate-defvar
(define-generator generate-defvar
  (predicate defvar-stmt?)
  (define (stmt)
    (string-append (generate-sequence generate " " (def-stmt-classes stmt))
                   (generate-type (defvar-stmt-type stmt) (def-stmt-name stmt))
                   " = "
                   (generate (defvar-stmt-definition stmt)))))

;; generate-defun
(define-generator generate-declfun
  (predicate declfun-stmt?)
  (define (a-stmt)
    (string-append (generate-sequence generate " " (decl-stmt-classes a-stmt))
                   (generate-type (declfun-stmt-return a-stmt)
                                  (decl-stmt-name a-stmt))
                   "("
                   (generate-arguments generate-argument ", " (declfun-stmt-arguments a-stmt))
                   ")")))

;; generate-defvar
(define-generator generate-declvar
  (predicate declvar-stmt?)
  (define (a-stmt)
    (string-append (generate-sequence generate " " (decl-stmt-classes a-stmt))
                   (generate-type (declvar-stmt-type a-stmt) (decl-stmt-name a-stmt)))))
;;
(define-generator generate-arr
  (predicate arr-expr?)
  (define (expr)
    (string-append (generate (arr-expr-type expr))
                   (if (arr-expr-size expr)
                       (string-append "[" (generate (arr-expr-size expr)) "]")
                       "[]"))))


;;
(define-generator generate-ptr
  (predicate ptr-expr?)
  (define (expr)
    (string-append (generate (ptr-expr-type expr)) " *")))

;;
(define-generator generate-return
  (predicate return-expr?)
  (define (expr)
    (string-append "return "
                   (if (return-expr-return expr)
                       (generate (return-expr-return expr))
                       ""))))

;; generate-call
(define-generator generate-call
  (predicate call-expr?)
  (define (expr)
    (string-append (generate (call-expr-caller expr))
                   (string-append "(" (generate-arguments generate ", " (call-expr-arguments expr)) ")"))))

;; generate-call
(define-generator generate-cast
  (predicate cast-expr?)
  (define (a-expr)
    (string-append "(("
                   (generate (cast-expr-type a-expr))
                   ")"
                   (generate (cast-expr-target a-expr))
                   ")")))

;;
(define-generator generate-unary
  (predicate unary-expr?)
  (define (expr)
    (define operator
      (case (unary-expr-operator expr)
        ;[(not) "!"]
        [(ref) "&"]
        [(deref) "*"]
        [(1++) "++"]
        [(1--) "--"]
        [else (symbol->string (unary-expr-operator expr))]))
    (string-append "("
                   (case (unary-expr-operator expr)
                     [(1++ 1--) (string-append (generate (unary-expr-operand expr)) operator)]
                     [else (string-append operator (generate (unary-expr-operand expr)))])
                   ")")))

;;
(define-generator generate-binary
  (predicate binary-expr?)
  (define (expr)
    (string-append "("
                   (generate-arguments generate
                                       (symbol->string (binary-expr-operator expr))
                                       (binary-expr-operands expr))
                   ")")))
;;
(define-generator generate-trinary
  (predicate trinary-expr?)
  (define (a-expr)
    (string-append "("
                   (generate (trinary-expr-condition a-expr))
                   " ? "
                   (generate (trinary-expr-left a-expr))
                   " : "
                   (generate (trinary-expr-right a-expr))
                   ")")))

;; generate-enum
(define-generator generate-enum
  (predicate enum-stmt?)
  (define (a-stmt)
    (define (generate-pair e)
      (if (symbol-expr? e)
          (generate e)
          (string-append (generate (pair-expr-left e))
                         " = "
                         (generate (pair-expr-right e)))))
    (string-append "enum "
                   (if (type-stmt-name a-stmt)
                       (generate (type-stmt-name a-stmt))
                       "")
                   (if (not (type-stmt-definition a-stmt))
                       ""
                       (string-append " {\n"
                                      (generate-sequence generate-pair "," (type-stmt-definition a-stmt))
                                      "}")))))

;;
;; generate-struct
(define-generator generate-struct
  (predicate struct-stmt?)
  (define (a-stmt)
    (define (generate-field e)
      (let ((items (tuple-expr-items e)))
        (string-append (generate-type (second items)
                                      (first items))
                       (if (< (length items) 4)
                           ""
                           (string-append " : "
                                          (generate (fourth items)))))))
    (string-append (symbol->string (struct-stmt-symbol a-stmt))
                   " "
                   (if (type-stmt-name a-stmt)
                       (generate (type-stmt-name a-stmt))
                       "")
                   (if (not (type-stmt-definition a-stmt))
                       ""
                       (string-append " {\n"
                                      (generate-sequence generate-field ";\n" (type-stmt-definition a-stmt))
                                      "}")))))

;; generate-deftype
(define-generator generate-deftype
  (predicate deftype-stmt?)
  (define (a-stmt)
    (string-append "typedef "
                   (generate-type (type-stmt-definition a-stmt)
                                  (type-stmt-name a-stmt)))))

;;
(define-generator generate-attribute
  (predicate attribute-expr?)
  (define (expr)
    (string-append (symbol->string (attribute-expr-attribute expr))
                   " "
                   (generate (attribute-expr-definition expr)))))

;; generate-type
(define (generate-type type name)
  (cond
    [(fptr-expr? type)
     (string-append (generate (fptr-expr-return type))
                    " (*"
                    (generate name)
                    ")("
                    (generate-arguments generate-argument ", " (fptr-expr-arguments type))
                    ")")]
    [(arr-expr? type)
     (string-append (generate (arr-expr-type type))
                    " "
                    (generate name)
                    "["
                    (if (arr-expr-size type)
                        (generate (arr-expr-size type))
                        "")
                    "]")]
    [else
     (string-append (generate type)
                    " "
                    (generate name))]))
                  

;; generate-int
(define-generator generate-int
  (predicate int-expr?)
  (define (expr)
    (number->string (atom-expr-data expr))))

;; generate-bool
(define-generator generate-bool
  (predicate bool-expr?)
  (define (expr)
    (if (atom-expr-data expr)
        "1"
        "0")))

;; char-convert
;; char char to c string
(define (char-convert c)
  (case c
    [(#\newline #\linefeed) "\\n"]
    [(#\null #\nul) "\\0"]
    [(#\space) " "]
    [(#\backspace) "\\b"]
    [(#\return) "\\r"]
    [(#\tab) "\\t"]
    [else (string c)]))

;; generate-char
(define-generator generate-char
  (predicate char-expr?)
  (define (a-expr)
    (string-append "'"
                   (char-convert (atom-expr-data a-expr))
                   "'")))

;; generate-float
(define-generator generate-float
  (predicate float-expr?)
  (define (expr)
    (string-append (number->string (atom-expr-data expr)) "f")))

;; generate-string
(define-generator generate-string
  (predicate string-expr?)
  (define (expr)
    (string-append "\""
                   (apply string-append
                          (map char-convert
                               (string->list (atom-expr-data expr))))
                   "\"")))

;; naming-convert
(define (naming-convert name)
  (apply string
         (map (lambda (c)
                (case c
                  [(#\-) #\_]
                  [else c]))
              (string->list name))))

;; generate-symbol
(define-generator generate-symbol
  (predicate symbol-expr?)
  (define (expr)
    (naming-convert
     (substring (symbol->string (atom-expr-data expr))
                (if (regexp-match #rx"c:.*" (symbol->string (atom-expr-data expr)))
                    2
                    0)))))

;;
(define-generator generate-keyword
  (predicate keyword-expr?)
  (define (exp)
    (naming-convert (keyword->string (atom-expr-data exp)))))
