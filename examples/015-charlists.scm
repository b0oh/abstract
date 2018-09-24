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

       (Y
        (lambda (self)
          (let ((f (lambda (x) (self (x x)))))
            (f f))))

       ;; logic

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

       ;; pairs

       (pair/make
        (lambda (first second pair)
          (pair first second)))

       (first
        (lambda (pair)
          (pair const)))

       (second
        (lambda (pair)
          (pair nil)))

       ;; natural numbers

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

       (0? nil?)

       (nat/leq?
        (lambda (num1 num2)
          (0? (- num1 num2))))

       (nat/eq?
        (lambda (num1 num2)
          (and (nat/leq? num1 num2) (nat/leq? num2 num1))))

       ;; lists

       (cons
        (lambda (head tail trans init)
          (trans head (tail trans init))))

       (head
        (lambda (elems)
          (elems const nil)))

       (tail
        (lambda (elems cons init)
          (let ((helper
                 (lambda (head tail trans)
                   (trans head (tail cons)))))

            (elems helper (const init) nil))))

       ;; end of the library

       (1 (inc 0))

       (2 (inc 1))

       (3 (inc 2))

       (4 (^ 2 2))

       (5 (inc 4))

       (6 (+ 2 4))

       (8 (^ 2 3))

       (32 (^ 2 5))

       (64 (^ 2 6))

       (72 (+ 64 8))

       (101 (+ (+ (+ 64 32) 4) 1))

       (108 (+ (+ (+ 64 32) 8) 4))

       (111 (+ (+ (+ (+ 64 32) 8) 6) 1))

       (H 72)

       (e 101)

       (l 108)

       (o 111)

       (hello (cons H (cons e (cons l (cons l (cons o nil)))))))

  hello)
