;(language Python
;  (token keyword (or "def" "return" "if" "else" "elif" "while" "for" "in" "import" "as" "class"))
;  (token identifier (concat (char-class "a-zA-Z_") (kleene-star (char-class "a-zA-Z0-9_"))))
;  (token operator (or "+" "-" "*" "/" "==" "!=" "="))
;  (token literal-string (concat #\" (kleene-star (char-class "^\"")) #\"))
;  (token literal-int (kleene-plus (char-class "0-9")))
;  (token comment (concat #\# (kleene-star (char-class "^\n")))))

(language Python 
  (token keyword (or "def" "return" "if" "else" "elif" "while" "for" "in" "import" "as" "class"))
  (token operator (or "+" "-" "*" "/" "==" "!=" "="))
  (token literal-int (kleene-plus (char-class "0-9")))
  (token comment (concat #\# (kleene-star (char-class "^\n\r"))))
  (token literal-string (literal "\"[^\"]*\""))
  (token identifier (concat (char-class "a-zA-Z_") (kleene-star (char-class "a-zA-Z0-9_")))))

(language Haskell
  (token keyword (or "let" "in" "if" "then" "else" "case" "of" "data" "type" "where"))
  (token identifier (concat (char-class "a-zA-Z_") (kleene-star (char-class "a-zA-Z0-9_'"))))
  (token operator (or "+" "-" "*" "/" "==" "/=" "="))
  (token literal-string (concat #\" (kleene-star (char-class "^\"")) #\"))
  (token literal-int (kleene-plus (char-class "0-9")))
  (token comment (concat #\- #\- (kleene-star (char-class "^\n")))))

(language Rust
  (token keyword (or "fn" "let" "mut" "if" "else" "match" "while" "loop" "for" "in" "struct" "enum"))
  (token identifier (concat (char-class "a-zA-Z_") (kleene-star (char-class "a-zA-Z0-9_"))))
  (token operator (or "+" "-" "*" "/" "==" "!=" "="))
  (token literal-string (concat #\" (kleene-star (char-class "^\"")) #\"))
  (token literal-int (kleene-plus (char-class "0-9")))
  (token comment (concat #\/ #\/ (kleene-star (char-class "^\n")))))
