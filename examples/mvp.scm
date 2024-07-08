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

(def (fib n) (let ((fib-iter (fn (a b count)) (if (= count 0)) a (fib-iter b (+ a b) (- count 1)))) (fib-iter 0 1 n)))
