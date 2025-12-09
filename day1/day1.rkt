#lang racket

(define input-path "input.txt")

(define lines
  (string-split (file->string input-path) #rx"\n")
  )

;;lines

(define vals
  (for/list ([line lines])
    (define val (string->number (substring line 1)))
    (case (string-ref line 0)
      ((#\L) (- val))
      (else val)
      )))

;; part 1
(let ()
  (define count 0)
  (define dial 50)
  (for ([val vals])
    (set! dial (modulo (+ dial val) 100))
    (when (= dial 0)
      (set! count (+ 1 count)))
    )

  count)

;; part 2
(let ()

  (define (range? val min max)
    (and (> val min)
         (< val max)))
  
  (define count 0)
  (define dial 50)
  (for ([val vals])
    (define new-dial (+ dial val))

    (cond
      ((and (not (= dial 0))
            (not (range? new-dial -1 101)))
       (define rots
         (or (and (> new-dial 0)
                  (quotient new-dial 100)
                  )
             (+ 1 (quotient (- new-dial) 100))
             ))
       (printf "~a/~a/~a (~a)~n" dial val new-dial rots)
       (set! count (+ rots count))
       )
      ((or (= new-dial 0) (= new-dial 100))
       (printf "~a/~a/~a (0)~n" dial val new-dial)
       (set! count (+ 1 count)))
      (else
       (printf "~a/~a/~a~n" dial val new-dial)
       ))
      
    (set! dial (modulo new-dial 100))
    
      
    )

  count)