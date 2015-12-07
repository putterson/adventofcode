#lang racket
(define vowels '("a" "e" "i" "o" "u"))

(define lines
  (call-with-input-file "input"
    (λ (in)
      (in-lines in))))

(define (nice-vowels? string)
  (#true))

(define (nice-doubles? string)
  (#true))

(define (nice-excluded? string)
  (#true))

(define (nice? string)
  (and (nice-vowels? string)
       (nice-doubles? string)
       (nice-excluded? string)))