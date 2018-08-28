(let* ((true (lambda (true false) true))
       (false (lambda (true false) false))

       (pair (lambda (first second pair) (pair first second)))
       (first (lambda (pair) (pair true)))
       (second (lambda (pair) (pair false)))

       (0 (lambda (succ zero) zero))
       (inc (lambda (num succ zero) (succ (num succ zero))))
       (+ (lambda (num1 num2 succ zero) (num1 succ (num2 succ zero))))
       (* (lambda (num1 num2 succ zero) (num1 (num2 succ) zero)))

       (paired_inc (lambda (num) (pair (second num) (inc (second num)))))
       (dec (lambda (num) (first (num paired_inc (pair 0 0)))))
       (- (lambda (num1 num2) (num2 dec num1)))

       (1 (inc 0))
       (2 (+ 1 1))
       (4 (* 2 2))
       (6 (+ 4 2))
       (8 (* 4 2))
       (16 (* 4 4))
       (10 (- 16 6)))
  10)
