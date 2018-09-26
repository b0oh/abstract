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

       (cons
        (lambda (head tail trans init)
          (trans head (tail trans init))))

       (head
        (lambda (elemes)
          (elems (lambda (head _tail) head) false)))

       (head
        (lambda (elems)
          (elems const false)))

       (tail
        (lambda (elems cons init)
          (let ((helper
                 (lambda (head tail pred)
                   (pred head (tail cons)))))

            (elems helper (const init) nil))))

       (1 (inc 0))

       (2 (inc 1))

       (3 (inc 2))

       (4 (inc 3))

       (nums (cons 1 (cons 2 (cons 3 (cons 4 nil)))))

       (fold-right (lambda (trans init elems) (elems trans init)))

       (empty?
        (lambda (l)
          (l (lambda (h t d) true) false)))

       (empty? (fold-right (lambda (h t) false) true))

       (product (fold-right * 1)))

  (if (empty? nums)
      WUT?
      piu))
;;      )(pair/make nums (product nums)))
