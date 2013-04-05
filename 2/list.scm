(define (seq low high step)
  (if (> low high)
    '()
    (cons low (seq (+ low step)
                   high
                   step))))

(define (sum l)
  (foldl + 0 l))

