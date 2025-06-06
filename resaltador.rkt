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
     (regexp-quote (cadr expr)))
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
      (map (λ (token)
             (let ((nombre-token (second token))
                   (exp (third token)))
               (cons nombre-token (regexp (sexp->regex exp)))))
           (cddr lenguaje))
      '()))


(define (argmin f lst)
  (foldl (λ (x y) (if (< (f x) (f y)) x y)) (car lst) (cdr lst)))

;; === Highlighting del archivo ===
(define (highlight text token-regexes)
  (define (loop t acc)
    (cond
      ((string=? t "") acc)
      (else
       (define all-matches
         (filter identity
                 (for/list ((rule token-regexes))
                   (define m (regexp-match-positions (cdr rule) t))
                   (and m (cons (car rule) m)))))

       (if (null? all-matches)
           (append acc (list t)) ; ningún match encontrado
           (let* ((best (argmin (lambda (m) (caar (cdr m))) all-matches)) ; match más cercano
                  (tag (car best))
                  (range (cdr best))
                  (s (caar range))
                  (e (cdar range)))
             (if (>= s e) ; prevenir matches vacíos
                 (loop (substring t 1) (append acc (list (substring t 0 1))))
                 (let* ((pre (substring t 0 s))
                        (tok (substring t s e))
                        (post (substring t e)))
                   (loop post
                         (append acc
                                 (if (string=? pre "")
                                     (list `(span ((class ,(symbol->string tag))) ,tok))
                                     (list pre `(span ((class ,(symbol->string tag))) ,tok))))))))))))
  (loop text '()))



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
