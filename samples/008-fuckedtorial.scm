(let* ((true (lambda (true false) true))
       (false (lambda (true false) false))

       (fix (lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x))))))

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
       (4 (* 2 2))
       (16 (* 4 4))

       (almost-factorial
        (lambda (fac)
          (lambda (num)
            (if (eq? num 0)
                1
                (* num (fac (- n 1)))))))


       (fac (fix almost-factorial)))

  (fac 1))
