#lang racket
(require openssl/md5)

(define key "ckczppom")

(define (find-hash prefix)
  (define (find-iter cur)
    (let ([in (~a key cur)])
      (if (string-prefix? (md5 (open-input-string in)) prefix)
          in
          (find-iter (+ cur 1)))))
  (find-iter 0))

(find-hash "00000")
(find-hash "000000")