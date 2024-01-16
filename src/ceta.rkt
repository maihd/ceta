#lang racket

;; Ceta programming languages by MaiHD

(require dynext/compile
         dynext/link
         "parser.rkt"
         "generator.rkt"
         "preprocessor.rkt")

;; version : (struct number number)
(struct version (major minor patch build))

;; version->string : version -> string
(define (version->string a-version)
  (string-append (number->string (version-major a-version))
                 "."
                 (number->string (version-minor a-version))))

;; application : (struct string version)
(struct application (name version))

;; application-intro : application -> string
(define (application-intro app)
  (string-append (application-name app)
                 " v"
                 (version->string (application-version app))))

(define ceta-application
  (application "ceta language"
               (version 1 0 0 38)))

;(displayln (application-intro ceta-application))

;; compile : s-expression -> string/
(define (compile code)
  (generate (parse (preprocess code))))

;; compile : path-string? path-string? -> any/c
;; run compiler
(define (compile-file in-file expect-out-file)
  ;;
  (define out-file
    (if expect-out-file
        expect-out-file
        (let ((splitted (string-split in-file ".")))
          (string-join
           (if (equal? (last splitted) "ceta")
               (list-set splitted (- (length splitted) 1) "c")
               (append splitted "c"))
           "."))))

  ;; open compile file
  (with-input-from-file in-file
    (lambda ()
      (with-output-to-file out-file #:exists 'replace
        (lambda ()
          (define (loop)
            (let ([buffer (read)])
              (if (eof-object? buffer)
                  (void)
                  (let ((compiled (compile buffer)))
                    (display compiled)
                    (unless (equal? (string-ref compiled 0) #\#)
                      (display ";"))
                    (newline)
                    (loop)))))
          (loop)))))

  ;; result message
  ;(displayln "compile <ceta> successfully!"))
  )

;; compile : path-string? path-string? -> any/c
;; run compiler
(define (compile-meta-file in-file expect-out-file)
  ;;
  (define splitted (string-split in-file "."))
  (define file-header
    (string-join
     (string-split
      (string-join
       (if (equal? (last splitted) "meta")
           (list-set splitted (- (length splitted) 1) "h")
           (append splitted "h"))
       "_")
      "-")
     "_"))
  (define out-file
    (if expect-out-file
        expect-out-file
        (string-join
         (if (equal? (last splitted) "meta")
             (list-set splitted (- (length splitted) 1) "h")
             (append splitted "h"))
         ".")))
  ;; open compile file
  (with-input-from-file in-file
    (lambda ()
      (with-output-to-file out-file #:exists 'replace
        (lambda ()
          (define (loop)
            (let ([buffer (read)])
              (if (eof-object? buffer)
                  (void)
                  (let ((compiled (compile buffer)))
                    (display compiled)
                    (unless (and (> (string-length compiled) 0)
                                 (equal? (string-ref compiled 0) #\#))
                      (display ";"))
                    (newline)
                    (loop)))))
          (displayln (string-append "#ifndef __" file-header "__"))
          (displayln (string-append "#define __" file-header "__"))
          (displayln "")
          (loop)
          (displayln "#endif")))))

  ;; result message
  ;(displayln "compile <meta> successfully!"))
  )

(define output-filename (make-parameter #false))

;; parse command line
(define-values (input-filename)
  (command-line #:program (application-name ceta-application)
                #:multi
                [("-o" "--output") file "Output file." (output-filename file)]
                #:args (input-filename)
                input-filename))

;; begin compile
(unless (file-exists? input-filename)
  (error (string-append "error: file not found, " input-filename)))
(if (equal? "meta" (last (string-split input-filename ".")))
    (compile-meta-file input-filename (output-filename))
    (compile-file input-filename (output-filename)))
(begin
  (when (file-exists? (string-append input-filename ".ceta"))
    (compile-file (string-append input-filename ".ceta") (output-filename)))
  (when (file-exists? (string-append input-filename ".meta"))
    (compile-meta-file (string-append input-filename ".meta") (output-filename))))