#lang racket
(define-struct box (l w h))

(define (read-in-box str)
  (let ([ns (cdr (regexp-match #px"(\\d+)x(\\d+)x(\\d+)" str))])
    (define (read-iter lst partial-box)
      (match lst
        ['() (partial-box)]
        [_ (read-iter (cdr lst) (curry partial-box (string->number (car lst))))])
      )
    (read-iter ns box)))

(define (wrapping-paper l w h)
  (let ([lw (* l w)]
        [lh (* l h)]
        [wh (* w h)])
    (let ([m (min lw lh wh)])
      (+ (* 2 lw) (* 2 lh) (* 2 wh) m)
      )))

(define (ribbon l w h)
  (let ([plw (+ (* 2 l) (* 2 w))]
        [plh (+ (* 2 l) (* 2 h))]
        [phw (+ (* 2 h) (* 2 w))])
    (let ([m (min plw plh phw)])
      (+ m (* l w h))
      )))

(define (calculate formula)  
  (call-with-input-file "input"
    (λ (in)
      (foldl + 0 
             (sequence->list
              (sequence-map (λ (line) 
                              (let ([b (read-in-box line)])
                                (let ([l (box-l b)]
                                      [w (box-w b)]
                                      [h (box-h b)])
                                  (formula l w h))))
                            (in-lines in)))))))
(calculate wrapping-paper)
(calculate ribbon)