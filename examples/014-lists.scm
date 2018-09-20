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

       ;; end of the library

       (cons/make
        (lambda (head tail trans init)
          (trans head (tail trans init))))

       (head
        (lambda (list)
          (list (lambda (head _tail) head) false)))

       (head
        (lambda (list)
          (list const false)))

       (tail
        (lambda (list cons init)
          (let ((helper
                 (lambda (head tail pred)
                   (pred head (tail cons)))))

            (list helper (const init) nil))))

       (1 (inc 0))

       (2 (inc 1))

       (3 (inc 2))

       (4 (inc 3))

       (nums (cons/make 1 (cons/make 2 (cons/make 3 (cons/make 4 nil)))))

       (fold-right (lambda (trans init list) (list trans init)))

       (product (fold-right * 1)))

  (pair/make nums (product nums)))
