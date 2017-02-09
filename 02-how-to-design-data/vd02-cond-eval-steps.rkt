#lang htdp/bsl

(cond [(> 1 2) "bigger"]
      [(= 1 2) "equal"]
      [(< 1 2) "smaller"])

(cond [#false "bigger"]
      [(= 1 2) "equal"]
      [(< 1 2) "smaller"])

(cond [(= 1 2) "equal"]
      [(< 1 2) "smaller"])

(cond [#false "equal"]
      [(< 1 2) "smaller"])

(cond [(< 1 2) "smaller"])

(cond [#true "smaller"])

"smaller"

