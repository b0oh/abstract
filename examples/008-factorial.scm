(let* ((identity
        (lambda (same)
          same))

       (const
        (lambda (always _never)
          always))

       (nil
        (lambda (_always never)
          never))

       (Y
        (lambda (g)
          (let ((f (lambda (x) (g (x x)))))
            (f f))))

       (true const)

       (false nil)

       (nil?
        (lambda (term)
          (term (const false) true)))

       (and
        (lambda (pred1 pred2)
          (pred1 pred2 pred1)))

       (if
        (lambda (pred true-clause false-clause)
          (pred true-clause false-clause)))

       (0 nil)

       (inc
        (lambda (num succ zero)
          (succ (num succ zero))))

       (+
        (lambda (num1 num2 succ zero)
          (num1 succ (num2 succ zero))))

       (dec
        (lambda (num succ zero)
          (num (lambda (g h) (h (g succ)))
               (const zero)
               identity)))

       (-
        (lambda (num1 num2)
          (num2 dec num1)))

       (0? nil?)

       (leq?
        (lambda (num1 num2)
          (0? (- num1 num2))))

       (nat/eq?
        (lambda (num1 num2)
          (and (leq? num1 num2) (leq? num2 num1))))

       (1 (inc 0))

       (2 (+ 1 1))

       (fac
        (Y (lambda (fac num)
             (if (nat/eq? num 0)
                 1
                 (* num (fac (- num 1))))))))

  (fac (+ 1 2)))
