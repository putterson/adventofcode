#lang racket

;Borrowed this macro from https://blog.jverkamp.com/2012/10/20/memoization-in-racket/
;Had to resort to non-pure memoization for performance, luckily everything else is pure and so the safety is maintained
(define-syntax define-memoized
  (syntax-rules ()
    [(_ (f args ...) bodies ...)
     (define f
       ; store the cache as a hash of args => result
       (let ([results (make-hash)])
         ; need to do this to capture both the names and the values
         (lambda (args ...)
           ((lambda vals
              ; if we haven't calculated it before, do so now
              (when (not (hash-has-key? results vals))
                (hash-set! results vals (begin bodies ...)))
              ; return the cached result
              (hash-ref results vals))
            args ...))))]))

(define (arithmetic-rshift n m)
  (arithmetic-shift n (- m)))

(define (parse-binary-op str)
  (match str
    ["AND" bitwise-and]
    ["OR" bitwise-ior]
    ["LSHIFT" (λ (n m) (bitwise-and 65535 (arithmetic-shift n m)))]
    ["RSHIFT" (λ (n m) (bitwise-and 65535 (arithmetic-rshift n m)))]))

(define (parse-unary-op str)
  (match str
    ;not implemented to simulate unsigned 16 bit int
    ["NOT" (λ (in) (- 65535 in))]))

(define-memoized (get-value state name)
  (match name
    [(pregexp #px"\\d+") (string->number name)]
    [_ (if (dict-has-key? state name)
           ((dict-ref state name) state)
           0)]))

(define (set-value state name proc)
  (dict-set state name proc))

(define (wire-port out proc)
  (λ (state) (set-value state out proc)))

(define (parse line)
  (match line
    [(pregexp #px"(\\w+) (AND|OR|LSHIFT|RSHIFT) (\\w+) -> (\\w+)" (list _ opa op opb out))
     (wire-port out (λ (state) ((parse-binary-op op) (get-value state opa) (get-value state opb))))]
    [(pregexp #px"(NOT) (\\w+) -> (\\w+)" (list _ op opa out))
     (wire-port out (λ (state) ((parse-unary-op op) (get-value state opa))))]
    [(pregexp #px"(\\w+) -> (\\w+)" (list _ in out))
     (wire-port out (λ (state) (get-value state in)))]
    [_ (λ (state) (define (error) ((display "Invalid instruction:")
                                   (display line)
                                   state))
         (error))]))

(define instructions (call-with-input-file "input"
                       (λ (in) (sequence->list (in-lines in)))))

(define end-state (foldl (λ (instruction state) ((parse instruction) state)) (hash) instructions))
(define a (get-value end-state "a"))
a
(define new-state ((wire-port "b" (λ (_) a)) end-state))
(get-value new-state "a")