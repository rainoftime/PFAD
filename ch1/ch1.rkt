#lang racket

;; Problem : Find the smallest free number


;; simple approch
(define (minfree xs)
  (stream-first 
   (stream-filter 
    (λ (x) (not (member x xs)))
    (in-naturals))))

(define (minform a n xs)
  (if (zero? n)
      a
      (let ([b (+ 1 a (quotient n 2))])
        (let-values ([(us vs) (partition (λ (x) (< x b)) xs)])
          (let ([m (length us)])
            (if (= m (- b a))
                (minform b (- n m) vs)
                (minform a m us)))))))

(define (minfree2 xs)
  (minform 0 (length xs) xs))

;; test 
(define xs (map string->number (string-split "08,23,09,00,12,11,01,10,13,07,41,04,14,21,05,17,03,19,02,06" ",")))
(minfree xs)
(minfree2 xs)

;; need more tests

