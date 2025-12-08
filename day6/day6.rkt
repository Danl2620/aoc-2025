#lang racket

;; need the handy transpose function from text-table/utils
(require text-table/utils)

(define input-path "input.txt")

(define lines
  (reverse
   (map (lambda (line)
          ;; append a space to make the last column like the others: ending in a space
          ;; before starting the next.
          (string-append line " "))
        (string-split (file->string input-path) #rx"\n")
        )))

;; extract just the operation symbols
(define operations
  (map string->symbol
       (string-split (first lines) #px"[[:space:]]+")
       ))

;; use the operations line to deduce the column widths (or "lens" short for lengths)
(define lens
  (map (lambda (str)
         (+ 0 (string-length str)))
       (string-split (first lines) #px"[*+]")
       ))

;; grab the columns of a line using the widths
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

;; get the columns of all the rows
(define chunks
  (let ([len (length operations)])
    (map (lambda (line)
           (define lst (string->list line))
           (split-line lst len 0)
           )
         (rest lines))))

;; some messy logic to reverse+transpose the columns and parse the numbers
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

