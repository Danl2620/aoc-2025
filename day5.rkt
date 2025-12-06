#lang racket

(define (get-fresh-and-available db-string
                                 #:verbose (verbose #f))
  (define-values (id-ranges id-available1)
    (splitf-at
     (string-split db-string #rx"\n")
     (lambda (str) (> (string-length str) 0))
     ))

  (define id-available
    (for/list ([id-str (cdr id-available1)])
      (string->number id-str)
      ))

  (define id-fresh
    (for/list ([r (in-list id-ranges)])
      (define rs (string-split r "-"))
      (list (string->number (first rs))
            (string->number (second rs))
            )))

  (when verbose
    (printf "fresh ids: ~a~navailable ids: ~a~n"
            id-fresh
            id-available
            ))
  (values id-fresh id-available))

(define (calc-fresh-ingredients db-string
                                #:verbose (verbose #f))

  (define-values (id-fresh id-available)
    (get-fresh-and-available db-string #:verbose verbose)
    )
  
  (length
   (for/list ([id (in-list id-available)]
              #:when (findf (lambda (range)
                              (and (>= id (first range))
                                   (<= id (second range))
                                   ))
                            id-fresh
                            ))
     id
     )))

(define db-string-test "3-5
10-14
16-20
12-18

1
5
8
11
17
32")



(define (get-fresh-count db-string
                         #:verbose (verbose #f))


  (define-values (id-fresh id-available)
    (get-fresh-and-available db-string #:verbose verbose)
    )

  

  
  (define count 0)
  (define fresh-map (make-hasheq))
  (for/list ([pair (in-list id-fresh)])
    (define start (first pair))
    (define end (second pair))
    (when verbose (printf "~a - ~a, " start end))
    (for ([ii (in-range start (+ end 1))])
      ;      (unless (hash-has-key? fresh-map ii)
      ;        (set! count (+ 1 count))
      ;        )
      (hash-set! fresh-map ii 1)
      ))

  (when verbose
    (printf "map: ~a~n" fresh-map))
  
  count)


(module+ problem1
  (calc-fresh-ingredients db-string-test #:verbose #t)

  (define db-string-aoc (file->string "input.txt"))

  (calc-fresh-ingredients db-string-aoc)
  )

(module+ problem2
  (get-fresh-count db-string-test #:verbose #t)
  (define db-string-aoc (file->string "input.txt"))

  (get-fresh-count db-string-aoc)
  )