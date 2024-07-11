;; Special Forms:
;; - def: define a variable
;; - let: bind variables in a scope
;; - list: create a list
;; - set: create a set
;; - map: create a map
;; - array: create an array
;; - byte-array: create a byte array
;; - apply: apply a function to a list of arguments
;; - eval: evaluate a term
;; - read: read a term from a string
;; - fn: create a lambda function
;; - and: short-circuiting logical and
;; - or: short-circuiting logical or
;; - quote: prevent evaluation of a term
;; - quasiquote: prevent evaluation of a term, except for unquoted terms
;; - unquote: evaluate a term in a quasiquote
;; - unquote-splicing: evaluate a term in a quasiquote and splice the result

;; Built-in Functions:
;; - +: add numbers
;; - -: subtract numbers
;; - *: multiply numbers
;; - /: divide numbers
;; - %: modulo
;; - =: equality
;; - <: less than
;; - >: greater than
;; - <=: less than or equal to
;; - >=: greater than or equal to
;; - head: get the first element of a list
;; - tail: get the rest of the elements of a list
;; - empty?: check if a list is empty
;; - pair: create a pair

;; Built-in data types:
;; - Num
;; - Real
;; - Rational
;; - BigRational
;; - Int
;; - BigInt
;; - Nat
;; - BigNat
;; - Byte
;; - Boolean
;; - List
;; - Set
;; - Map
;; - Array
;; - ByteArray
;; - Symbol
;; - Char
;; - String

(def x 10)
x
; => 10

; Function definition
(def (add x y) (+ x y))

; Function call
(add 1 2)
; => 3

; Let binding
(let ((x 10) (y 20)) (+ x y))
; => 30

; List creation
(list 1 2 3)

; Lambda function
((fn (x) (+ x 1)) 1)

; Logical and
(and true false)
; => false

; Logical or
(or true false)
; => true

; Quote
(quote (+ 1 2)) ; or
'(+ 1 2)
; => (+ 1 2)

; Quasiquote/unquote
`(1 2 ,(+ 1 2))
; => (1 2 3)

; Unquote-splicing
`(1 2 ,@(list 3 4))
; => (1 2 3 4)

; Symbol
'symbol
; => symbol

; Keyword symbol
:keyword
; => :keyword

; byte array
(byte-array 1 2 3) ; or
#u8(1 2 3)
; => #u8(1 2 3)

(def (compose f g x) (f (g x)))

(let ((id (fn (x) x))) (id 1))

(def (fib n) (let ((loop (fn (a b i) (if (= i 0) a (loop b (+ a b) (- i 1)))))) (loop 0 1 n)))

(def (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))

(def (fact n) (let ((loop (fn (n acc) (if (= n 0) acc (loop (- n 1) (* n acc)))))) (loop n 1)))

(def (map f xs) (if (empty? xs) '() (pair (f (head xs)) (map f (tail xs)))))

(def (foldr f z xs) (if (empty? xs) z (f (head xs) (foldr f z (tail xs)))))

; is_prime: check if a number is prime
(def (is_prime n)
  (if (< n 2)
      false
      (let loop ((i 2))
        (if (< i n)
            (if (= (% n i) 0)
                false
                (loop (+ i 1)))
            true))))
          
; aks: check if a number is prime using the AKS primality test
(def (aks n)
  (if (< n 2)
      false
      (let outer ((r 2))
        (while (<= r (sqrt n))
          (if (= (% n r) 0)
              false
              (let inner ((a 1) (b 1))
                (while (<= a (sqrt n))
                  (if (= (+ (* a a) (* b b)) n)
                      true
                      (inner (+ a 1) (+ b 1)))))
              (outer (+ r 1))))
        true)))

; sieve of eratosthenes
(def (sieve n)
  (let ((s (array n true)))
    (set s 0 false)
    (set s 1 false)
    (let loop ((i 2))
      (if (< i n)
          (if (get s i)
              (let ((j (* i i)))
                (while (< j n)
                  (set s j false)
                  (set j (+ j i))))
              '())
          (loop (+ i 1))))
    (foldr (fn (i acc) (if (get s i) (pair i acc) acc)) '() (range 2 n))))

(def (fib n)
  (let ((loop (fn (a b i)
                (if (= i 0)
                    a
                    (loop b (+ a b) (- i 1))))))
    (loop 0 1 n)))

(def (map f xs)
  (if (empty? xs)
      '()
      (pair (f (head xs)) (map f (tail xs)))))
  
(def (foldr f z xs)
  (if (empty? xs)
      z
      (f (head xs) (foldr f z (tail xs)))))

`(1 2 ,@(map id '(3 4)))

(macro (defn name args body) `(def ,name (fn ,args ,body)))

