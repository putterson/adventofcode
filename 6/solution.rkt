#lang racket

(struct light (x y on) #:transparent
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (and (equal?-recur (light-x a) (light-x b))
          (equal?-recur (light-y a) (light-y b))))
   (define (hash-proc a hash-recur)
     (+ (hash-recur (light-x a))
        (* 3 (hash-recur (light-y a)))))
   (define (hash2-proc a hash2-recur)
     (+ (hash2-recur (light-x a))
        (hash2-recur (light-y a))))])

(define (init-grid level)
  (flatten (map
            (λ (x)
              (map
               (λ (y)
                 (light x y level))
               (range 0 1000)))
            (range 0 1000))))



(define (create-light-rule action x1 y1 x2 y2)
  (λ (l)
    (let ([lx (light-x l)]
          [ly (light-y l)])
      (if (and (<= x1 lx x2)
               (<= y1 ly y2))
          (action l)
          l))))


(define thousand-regex #px"(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)")
(define ten-regex #px"(turn on|turn off|toggle) (\\d)\\d*,(\\d)\\d* through (\\d)\\d*,(\\d)\\d*")

(define (map-light l trans)
  (light (light-x l) (light-y l) (trans (light-on l))))

(define (turn-on l)
  (map-light l (λ _ #t)))

(define (turn-off l)
  (map-light l (λ _ #f)))

(define (toggle l)
  (map-light l (λ (on) (not on))))

(define (old-getaction string)
  (match string
    ["turn on" turn-on]
    ["turn off" turn-off]
    ["toggle" toggle]
    [_ (λ (x) x)]))

(define (turn-up l)
  (map-light l (λ (on) (+ on 1))))

(define (turn-down l)
  (map-light l (λ (on) (max 0 (- on 1)))))

(define (turn2-up l)
  (map-light l (λ (on) (+ on 2))))

(define (new-getaction string)
  (match string
    ["turn on" turn-up]
    ["turn off" turn-down]
    ["toggle" turn2-up]
    [_ (λ (x) x)]))

(define (get-rule-applier getaction)
  (define (apply-rule rule grid)
    (let ([parse (regexp-match thousand-regex rule)])
      (match parse
        [#f
         (display "Bad rule")
         grid]
        [_
         (let ([rule (cdr parse)])
           (let ([action (getaction (list-ref rule 0))]
                 [x1 (string->number (list-ref rule 1))]
                 [y1 (string->number (list-ref rule 2))]
                 [x2 (string->number (list-ref rule 3))]
                 [y2 (string->number (list-ref rule 4))])
             (map (create-light-rule action x1 y1 x2 y2) grid)))])))
  apply-rule)

(define (lights-on g)
  (length
   (filter
    (λ (l) (light-on l))
    g)))

(define (lights-brightness g)
  (define (sum-light light accum)
    (+ accum (light-on light)))
  (foldl sum-light 0 g))

(define (calc apply-rule grid measure)
  (let ([finalgrid   (call-with-input-file "input"
                       (λ (in) 
                         (foldl apply-rule grid
                                (sequence->list (in-lines in)))))])
    (measure finalgrid)))
;Warning: I wrote this to be very innefficient. Each call takes ~1min
;(calc (get-rule-applier old-getaction) (init-grid #f) lights-on)
;(calc (get-rule-applier new-getaction) (init-grid 0) lights-brightness)
