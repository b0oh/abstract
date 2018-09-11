(let* ((true (lambda (true false) true))
       (false (lambda (true false) false))
       (and (lambda (pred1 pred2) (pred1 pred2 pred1)))
       (if (lambda (pred true-clause false-clause) (pred true-clause false-clause)))

       (fix (lambda (g) ((lambda (x) (g (x x))) (lambda (x) (g (x x))))))

       (pair (lambda (first second pair) (pair first second)))
       (first (lambda (pair) (pair true)))
       (second (lambda (pair) (pair false)))

       (0 (lambda (succ zero) zero))
       (inc (lambda (num succ zero) (succ (num succ zero))))
       (+ (lambda (num1 num2 succ zero) (num1 succ (num2 succ zero))))
       (* (lambda (num1 num2 succ zero) (num1 (num2 succ) zero)))

       (paired-inc (lambda (num) (pair (second num) (inc (second num)))))
       (dec (lambda (num) (first (num paired-inc (pair 0 0)))))
       (- (lambda (num1 num2) (num2 dec num1)))
       (0? (lambda (num) (num (lambda (_) false) true)))
       (leq? (lambda (num1 num2) (0? (- num1 num2))))
       (eq? (lambda (num1 num2) (and (leq? num1 num2) (leq? num2 num1))))

       (1 (inc 0))
       (2 (+ 1 1))

       (fac (fix (lambda (fac num)
                   (if (eq? num 0)
                       1
                       (* num (fac (- num 1))))))))

  (fac (+ 1 2)))
