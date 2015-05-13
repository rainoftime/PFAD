#lang racket

;; Problem : Find the smallest free number


;; simple approch
(define (minfree xs)
  (stream-first 
   (stream-filter 
    (λ (x) (not (member x xs)))
    (in-naturals))))


;; test 
(define xs (map string->number (string-split "08,23,09,00,12,11,01,10,13,07,41,04,14,21,05,17,03,19,02,06" ",")))
(minfree xs)