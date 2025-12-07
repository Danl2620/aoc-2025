#lang racket

(define input-path "input.txt")

(define lines (string-split (file->string input-path) #rx"\n"))

(define groups (for/list ([line (in-list lines)])
                 (string-split line #px"[[:blank:]]+")
                 ))

(printf "groups: ~a~n" groups)

(define expressions
  (for/list ([index (in-range (length (first groups)))])
    (reverse (for/list ([group (in-list groups)])
               (define token-str (list-ref group index))
               (case token-str
                 (("*") 'mul)
                 (("+") 'add)
                 (else (string->number token-str)))
               ))))

(printf "expressions: ~a~n" expressions)

(for/sum ([expr expressions])
  (case (first expr)
    ((mul) (apply * (rest expr)))
    ((add) (apply + (rest expr)))
    ))


