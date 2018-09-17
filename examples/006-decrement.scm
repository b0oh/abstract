(let* ((identity
        (lambda (same)
          same))

       (const
        (lambda (always _never)
          always))

       (nil
        (lambda (_always never)
          never))

       (make-pair
        (lambda (first second pair)
          (pair first second)))

       (first
        (lambda (pair)
          (pair const)))

       (second
        (lambda (pair)
          (pair nil)))

       (0
        (lambda (succ zero)
          zero))

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
       (another-dec
        (lambda (num)
          (let ((paired-inc
                 (lambda (num)
                   (make-pair (second num) (inc (second num))))))
            (first (num paired-inc (make-pair 0 0))))))

       (1 (inc 0))

       (2 (+ 1 1))

       (4 (* 2 2))

       (256 (^ 4 4)))

  (dec 256))
