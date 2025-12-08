#lang racket

;; need the handy transport function from text-table/utils
(require text-table/utils)

(define input-path "input.txt")

(define lines
  (reverse
   (map (lambda (line)
          (string-append line " "))
        (string-split (file->string input-path) #rx"\n")
        )))

(define operations
  (map string->symbol
       (string-split (first lines) #px"[[:space:]]+")
       ))

(define lens
  (map (lambda (str)
         (+ 0 (string-length str)))
       (string-split (first lines) #px"[*+]")
       ))

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

(define chunks
  (let ([len (length operations)])
    (map (lambda (line)
           (define lst (string->list line))
           (split-line lst len 0)
           )
         (rest lines))))


(define expressions
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
                            )))))))))


;; calculate each column and add the results!
(apply +
       (map (lambda (expr)
              (case (first expr)
                ((*) (apply * (rest expr)))
                ((+) (apply + (rest expr)))
                ))
            expressions))

