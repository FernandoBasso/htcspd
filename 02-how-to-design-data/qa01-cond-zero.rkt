#lang htdp/bsl

(define (mag2 x)
  (cond [(< x 0) "negative"]
        [(= x 0) "zero"]
        [else "positive"]))

(mag2 0)

(cond [#false "negative"]
      [(= 0 0) "zero"]
      [else "positive"])

(cond [(= 0 0) "zero"]
      [else "positive"])

(cond [#true "zero"]
      [else "positive"])

"zero"

