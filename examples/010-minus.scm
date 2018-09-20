(let* (;; copy of our small library

       (identity
        (lambda (same)
          same))

       (const
        (lambda (always _never)
          always))

       (nil
        (lambda (_always never)
          never))

       (true const)

       (false nil)

       (and
        (lambda (pred1 pred2)
          (pred1 pred2 pred1)))

       (or
        (lambda (pred1 pred2)
          (pred1 pred1 pred2)))

       (if
        (lambda (pred? then-clause else-clause)
          (pred? then-clause else-clause)))

       (not
        (lambda (pred?)
          (if pred? false true)))

       (nil?
        (lambda (term)
          (term (const false) true)))

       (pair/make
        (lambda (first second pair)
          (pair first second)))

       (first
        (lambda (pair)
          (pair const)))

       (second
        (lambda (pair)
          (pair nil)))

       ;; end of the library

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
