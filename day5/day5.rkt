#lang racket

(require
  racket/format)

;; translate the db string to the list of fresh ingredient ranges
;; and the list of available ingredients
(define (get-fresh-and-available db-string
                                 #:verbose (verbose #f))

  ;; split the incoming string into lines, then split
  ;; the lines into list of ranges and list of available ingredients
  (define-values (id-ranges id-available1)
    (splitf-at
     (string-split db-string #rx"\n")
     (lambda (str) (> (string-length str) 0))
     ))

  ;; convert the ranges from strings to numbers of the form:
  ;;   (list start end)
  (define id-fresh
    (for/list ([r (in-list id-ranges)])
      (define rs (string-split r "-"))
      (list (string->number (first rs))
            (string->number (second rs))
            )))

  ;; convert the ingredient list from strings to numbers
  (define id-available
    (for/list ([id-str (cdr id-available1)])
      (string->number id-str)
      ))
  
  (when verbose
    (printf "fresh ids: ~a~navailable ids: ~a~n"
            id-fresh
            id-available
            ))
  
  ;; return the two lists
  (values id-fresh id-available))

;; how many available ingredients are fresh?
(define (calc-fresh-ingredients db-string
                                #:verbose (verbose #f))

  ;; get the lists
  (define-values (id-fresh id-available)
    (get-fresh-and-available db-string #:verbose verbose)
    )

  ;; for every ingredient ID, for every range, if the ID falls within
  ;; the range keep it in the list. then return the length of the list.
  ;; NOTE: this does more work than necessary since it creates a list
  ;;       of fresh ingredients only to count the length and throw it
  ;;       away.
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


;; how many possible ingredient IDs are considered to be fresh?
(define (get-fresh-count db-string
                         #:verbose (verbose #f)
                         #:width (width 2))

  ;; extract the fresh ranges and the list of ingredients
  ;; note: we will ignore the list of ingredients
  (define-values (id-fresh unused)
    (get-fresh-and-available db-string)
    )

  ;; first sort the ranges by starts.
  (define id-fresh-sorted
    (sort id-fresh <
          #:key first))

  (when verbose
    (printf "sorted: ~a~n~n" id-fresh-sorted)
    )

  ;; Iterate from the first start to the last end, skipping
  ;; indices covered in previous ranges and keeping a count
  ;; along the way.
  (define count 0)
  (define index (first (first id-fresh-sorted)))
  (for/list ([pair (in-list id-fresh-sorted)])
    (define start
      (max (first pair) index))

    ;; consider the end +1 since the range is inclusive
    (define end
      (+ 1 (second pair)))

    ;; in the cases of skipping covered indices this range will
    ;; be 0 or negative so it's correct to skip the range
    (when (> end start)

      ;; calculate the delta of this range, add it to the count
      ;; and update the current index
      (define delta (- end start))
      (set! count (+ count delta))
      (set! index end)
      
      (when verbose 
        (printf "(~a -~a): d ~a, i ~a ~ c ~a~n"
                (~a start #:align 'right #:width (+ width 2))
                (~a end  #:align 'right #:width (+ width 2))
                (~a delta  #:align 'right #:width width)
                (~a index  #:align 'right #:width width)
                (~a count  #:align 'right #:width width)
                ))))

  count)


(module+ problem1
  ;; test case
  (define db-string-test (file->string "test-input.txt"))
  (calc-fresh-ingredients db-string-test #:verbose #t)

  ;; the first part of the problem
  (define db-string-aoc (file->string "input.txt"))
  (calc-fresh-ingredients db-string-aoc)
  )

(module+ problem2
  ;; test cases
  (define db-string-test (file->string "test-input.txt"))
  (define db-string-test-2 (file->string "test-input-2.txt"))
  (get-fresh-count db-string-test #:verbose #t)
  (get-fresh-count db-string-test-2 #:verbose #t)

  ;; the second part of the problem
  (define db-string-aoc (file->string "input.txt"))
  (get-fresh-count db-string-aoc #:verbose #f #:width 16)
  )
