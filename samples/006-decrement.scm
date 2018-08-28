(let* ((identity (lambda (same) same))
       (const (lambda (always _any) always))
       (fix (lambda (g) ((lambda (x) (g (x x))) (lambda (x) (g (x x))))))

       (true const)
       (false (const identity))

       (pair (lambda (first second pair) (pair first second)))
       (first (lambda (pair) (pair true)))
       (second (lambda (pair) (pair false)))

       (0 (lambda (succ zero) zero))
       (+1 (lambda (num succ zero) (succ (num succ zero))))
       (+ (lambda (num1 num2 succ zero) (num1 succ (num2 succ zero))))
       (* (lambda (num1 num2 succ zero) (num1 (num2 succ) zero)))

       (paired+1 (lambda (num) (pair (second num) (+1 (second num)))))
       (-1 (lambda (num) (first (num paired+1 (pair 0 0)))))

       (-1 (lambda (num succ zero)
            (num (lambda (g h) (h (g succ)))
                 (lambda (_) zero)
                 (lambda (u) u))))
       (- (lambda (num1 num2) (num2 -1 num1)))

       (1 (+1 0))
       (2 (+ 1 1))
       (4 (* 2 2))
       (16 (* 4 4)))
  (-1 16))
