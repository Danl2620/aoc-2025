#lang racket

(require text-table/utils)


(define input-path "test-input.txt")

(define lines
  (reverse
   (map (lambda (line)
          (string-append line " "))
        (string-split (file->string input-path) #rx"\n")
        )))

lines

;;(define row-count (- (length lines) 1))

(define operations
  (map string->symbol
       (string-split (first lines) #px"[[:space:]]+")
       ))

operations


(define lens
  (map (lambda (str)
         (+ 0 (string-length str)))
       (string-split (first lines) #px"[*+]")
       ))

lens

(define (split-line line len index)
  (cond
    ((>= index len)
     line)
    (else
     (define-values (el rst)
       (split-at line
                 (list-ref lens index))
       )
     (cons el (split-line (rest rst) len (+ index 1)))
     )))

#;(module+ test
    (define lst (string->list "  6 98   215 314    1 "))
    (split-line lst 5 0)
    )



(define chunks
  (let ([len (length operations)])
    (map (lambda (line)
           (define lst (string->list line))
           (split-line lst len 0)
           )
         (rest lines))))

chunks

(for/list ([index (in-range (length operations))])
  (cons (list-ref operations index)
        (map (compose string->number string-trim) 
             (map list->string
                  (transpose
                   (map reverse
                        (reverse
                         (map
                          (lambda (chunk) (list-ref chunk index))
                          chunks
                          ))))))))

#;(printf "lines: ~a~n" lines)

#;(define groups (for/list ([line (in-list lines)])
                   (string-split line #px"[[:blank:]]")
                   ))

#;(printf "groups: ~a~n" groups)

; (define expressions
;   (for/list ([index (in-range (length (first groups)))])
;     (reverse (for/list ([group (in-list groups)])
;                (define token-str (list-ref group index))
;                (case token-str
;                  (("*") 'mul)
;                  (("+") 'add)
;                  (else (string->number token-str)))
;                ))))

; (printf "expressions: ~a~n" expressions)

; (for/sum ([expr expressions])
;   (case (first expr)
;     ((mul) (apply * (rest expr)))
;     ((add) (apply + (rest expr)))
;     ))
