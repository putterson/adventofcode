#lang racket

(struct pt (x y) #:transparent)
(define (show-pt p)
                     (print (pt-x p))
                     (display ", ")
                     (print (pt-y p))
                     (newline))

(define (move-pt p char)
  (match char
    [#\< (pt (- (pt-x p) 1) (pt-y p))]
    [#\> (pt (+ (pt-x p) 1) (pt-y p))]
    [#\^ (pt (pt-x p) (+ (pt-y p) 1))]
    [#\v (pt (pt-x p) (- (pt-y p) 1))]
    [_ p]))

(define (parse in)
  (string->list (car (sequence->list (in-lines in)))))

(define (all in) in)

(define (evens lst)
  (if (or (null? lst)
          (null? (cdr lst)))
      '()
      (cons (cadr lst)
            (evens (cddr lst)))))

(define (odds lst)
  (if (null? lst)
      '()
      (if (null? (cdr lst))
          lst
          (cons (car lst)
                (odds (cddr lst))))))

(define (visited consume) 
(call-with-input-file "input"
  (λ (in)
    (let ([lst (consume (parse in))])
      (define (calc-iter visited pt directions)
        (let ([new-visited (set-add visited pt)])
          (match directions
            ['() new-visited]
            [_ (let ([new-pt (move-pt pt (car directions))])
                 (calc-iter new-visited new-pt (cdr directions)))]
            )))
      (calc-iter (set) (pt 0 0) lst)))))

(length (set->list (visited all)))
(length (set->list (set-union (visited evens) (visited odds))))