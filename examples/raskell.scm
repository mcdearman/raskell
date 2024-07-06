(def (span start end) {:start start :end end})

(def (token kind span) {:kind kind :span span})

(def (lexer src) {:src src :pos 0})

(def (matches? obj . cases) 
  (if (empty? cases) 
    false 
    (or (= obj (head cases)) (matches? obj (tail cases)))))

(def (whitespace? ch) (matches? ch " " "\t" "\n" "\r"))

(def (in-range? ch start end) 
  (and (>= ch start) (<= ch end)))

(def (digit? ch) (in-range? ch "0" "9"))

(def (is-alpha? ch) 
  (or (in-range? ch "a" "z") (in-range? ch "A" "Z")))

(def (is-alphanumeric? ch) 
  (or (is-alpha? ch) (digit? ch)))

(def (peek-char lexer) 
  (if (< (Map.get lexer pos) (String.length (Map.get lexer src)))
      (String.get (Map.get lexer src) (Map.get lexer pos))
      :nil))
