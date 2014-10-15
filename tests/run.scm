(use blink test)

(test-begin "blink")

(define dev (car blink-devices))

(test-assert dev)

(test-end)
(test-exit)
