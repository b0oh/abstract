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

       (comp
        (lambda (fun1 fun2 arg)
          (fun1 (fun2 arg))))

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

       (fold-right
        (lambda (trans init elems)
          (elems trans init)))

       (map
        (lambda (fun)
          (fold-right (comp cons fun) nil)))

       ;; end of the library

       (char/lowercase?
        (lambda (char)
          (and
           (not (nat/leq? char (dec #\a)))
           (nat/leq? char #\z))))

       (char/uppercase
        (lambda (char)
          (if (char/lowercase? char)
              (- char #\space)
              char)))

       (string/uppercase (map char/uppercase))

       (hello (list #\H #\e #\l #\l #\o)))

  (string/uppercase hello))
