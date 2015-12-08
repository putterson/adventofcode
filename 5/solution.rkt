#lang racket
(define vowels (list #\a #\e #\i #\o #\u))
(define (member? item list) (match (member item list)
                              [#f #f]
                              [_  #t]))

(define excluded (list "ab" "cd" "pq" "xy"))

(define (nice-vowels? string)
  (define (vowel-member? char) (member? char vowels))
  (>
   (length
    (filter vowel-member?
            (string->list (string-downcase string))))
   2))

(define (nice-doubles? string)
  (regexp-match? #px"(.)\\1" (string-downcase string)))

(define (nice-excluded? string)
  (define (matches-string? pattern) (regexp-match? pattern string))
  (match (ormap matches-string? excluded)
    [#t #f]
    [_  #t]))

(define (nice-double-pairs? string)
  (regexp-match? #px"(..).*\\1" (string-downcase string)))

(define (nice-sandwich? string)
  (regexp-match? #px"(.).\\1" (string-downcase string)))

(define (nice? rules string)
  (define (string-nice? rule)
    (rule string))
  (andmap string-nice? rules))

(define (old-nice? string)
  (nice? (list nice-vowels? nice-doubles? nice-excluded?) string))

(define (new-nice? string)
  (nice? (list nice-double-pairs? nice-sandwich?) string))

(call-with-input-file "input"
  (λ (in)
    (length (filter old-nice? (sequence->list (in-lines in))))))

(call-with-input-file "input"
  (λ (in)
    (length (filter new-nice? (sequence->list (in-lines in))))))