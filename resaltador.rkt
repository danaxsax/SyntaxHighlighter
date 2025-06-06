#lang racket
(require xml)

;; === sexp->regex ===
(define (sexp->regex expr)
  (cond
    ((string? expr)
     (regexp-quote expr))
    ((and (list? expr) (eq? (car expr) 'char-class))
     (string-append "[" (cadr expr) "]"))
    ((and (list? expr) (eq? (car expr) 'char))
     (regexp-quote (string (cadr expr))))
    ((and (list? expr) (eq? (car expr) 'literal))
     (cadr expr))  ; No escapar literales que ya son regex
    ((and (list? expr) (eq? (car expr) 'concat))
     (apply string-append (map sexp->regex (cdr expr))))
    ((and (list? expr) (eq? (car expr) 'or))
     (string-append "(" (string-join (map sexp->regex (cdr expr)) "|") ")"))
    ((and (list? expr) (eq? (car expr) 'kleene-star))
     (string-append "(" (sexp->regex (cadr expr)) ")*"))
    ((and (list? expr) (eq? (car expr) 'kleene-plus))
     (string-append "(" (sexp->regex (cadr expr)) ")+"))
    ((and (list? expr) (eq? (car expr) 'optional))
     (string-append "(" (sexp->regex (cadr expr)) ")?"))
    ((and (list? expr) (eq? (car expr) 'group))
     (string-append "(" (sexp->regex (cadr expr)) ")"))
    ((and (list? expr) (eq? (car expr) 'any))
     ".")
    (else "")))

;; === Leer tokens de archivo y construir regex ===
(define (cargar-lenguaje nombre)
  (define datos
    (call-with-input-file "tokens.rkt"
      (lambda (in)
        (let loop ((lst '()))
          (let ((exp (read in)))
            (if (eof-object? exp)
                lst
                (loop (cons exp lst))))))))
  (define lenguaje
    (findf (λ (x) (and (list? x) (equal? (second x) nombre))) datos))
  (if lenguaje
      ; Crear tokens con orden de prioridad: keywords, operators, strings, comments, numbers, identifiers
      (let ((tokens (map (λ (token)
                           (let ((nombre-token (second token))
                                 (exp (third token)))
                             (cons nombre-token (regexp (sexp->regex exp)))))
                         (cddr lenguaje))))
        (append 
         (filter (lambda (t) (eq? (car t) 'keyword)) tokens)
         (filter (lambda (t) (eq? (car t) 'operator)) tokens)  
         (filter (lambda (t) (eq? (car t) 'literal-string)) tokens)
         (filter (lambda (t) (eq? (car t) 'comment)) tokens)
         (filter (lambda (t) (eq? (car t) 'literal-int)) tokens)
         (filter (lambda (t) (eq? (car t) 'identifier)) tokens)))
      '()))


(define (argmin f lst)
  (foldl (λ (x y) (if (< (f x) (f y)) x y)) (car lst) (cdr lst)))

(define (argmax f lst)
  (foldl (λ (x y) (if (> (f x) (f y)) x y)) (car lst) (cdr lst)))

;; === Highlighting del archivo ===
(define (highlight text token-regexes)
  (define (tokenize-line line)
    (define (loop remaining acc)
      (cond
        ((string=? remaining "") acc)
        (else
         (define matches
           (filter (lambda (x) x)
                   (map (lambda (rule)
                          (let ((m (regexp-match-positions (cdr rule) remaining)))
                            (if (and m (= (caar m) 0))
                                (list (car rule) (caar m) (cdar m))
                                #f)))
                        token-regexes)))
         
         (if (null? matches)
             ; No hay matches, avanzar un carácter
             (loop (substring remaining 1) 
                   (append acc (list (substring remaining 0 1))))
             ; Tomar el match más largo
             (let* ((best (argmax (lambda (m) (- (third m) (second m))) matches))
                    (tag (first best))
                    (start (second best))
                    (end (third best))
                    (token (substring remaining start end))
                    (rest (substring remaining end)))
               (loop rest 
                     (append acc (list `(span ((class ,(symbol->string tag))) ,token)))))))))
    (loop line '()))
  
  ; Procesar línea por línea para manejar comentarios correctamente
  (define lines (string-split text "\n" #:trim? #f))
  (define processed-lines
    (map (lambda (line) 
           (if (string=? line "")
               (list "\n")
               (append (tokenize-line line) (list "\n"))))
         lines))
  
  ; Eliminar el último \n
  (let ((result (apply append processed-lines)))
    (if (and (not (null? result)) (string=? (last result) "\n"))
        (reverse (cdr (reverse result)))
        result)))



;; === HTML Output ===
(define (html-out chunks)
  (list 'html
    (list 'head
      (list 'style '((type "text/css"))
            "body { font-family: monospace; white-space: pre; }
             .keyword { color: blue; font-weight: bold; }
             .identifier { color: darkgreen; }
             .literal-int { color: red; }
             .literal-string { color: orange; }
             .operator { color: purple; }
             .comment { color: gray; font-style: italic; }"))
    (cons 'body chunks)))

;; === Ejecutar resaltado para un archivo y lenguaje dado ===
(define (resaltar-archivo archivo lenguaje)
  (define contenido (file->string archivo))
  (define token-regexes (cargar-lenguaje lenguaje))
  (for-each (λ (r) (printf "~a: ~a\n" (car r) (cdr r))) token-regexes)
  (define chunks (highlight contenido token-regexes))
  (define html (html-out chunks))
  (define salida (string-append (symbol->string lenguaje) "-highlight.html"))
  (with-output-to-file salida #:exists 'replace
    (lambda () (display (xexpr->string html))))
  (printf "Archivo generado: ~a\n" salida))

;; === Prueba ===
(resaltar-archivo "example.py" 'Python)
(resaltar-archivo "example.hs" 'Haskell)
(resaltar-archivo "example.rs" 'Rust)
