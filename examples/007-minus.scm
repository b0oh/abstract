(let* ((identity
        (lambda (same)
          same))

       (const
        (lambda (always _never)
          always))

       (nil
        (lambda (_always never)
          never))

       (0 nil)

       (inc
        (lambda (num succ zero)
          (succ (num succ zero))))

       (+
        (lambda (num1 num2 succ zero)
          (num1 succ (num2 succ zero))))

       (*
        (lambda (num1 num2 succ zero)
          (num1 (num2 succ) zero)))

       (^
        (lambda (num1 num2)
          (num2 num1)))

       (dec
        (lambda (num succ zero)
          (num (lambda (g h) (h (g succ)))
               (const zero)
               identity)))
       (-
        (lambda (num1 num2)
          (num2 dec num1)))

       (1 (inc 0))

       (2 (+ 1 1))

       (4 (* 2 2))

       (6 (+ 4 2))

       (256 (^ 4 4)))

  (- 256 6))
