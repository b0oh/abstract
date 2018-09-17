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

       (if
        (lambda (pred true-clause false-clause)
          (pred true-clause false-clause)))

       (make-pair
        (lambda (first second pair)
          (pair first second)))

       (first
        (lambda (pair)
          (pair const)))

       (second
        (lambda (pair)
          (pair nil)))

       (make-cons
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

       (0 nil)

       (0? nil?)

       (inc
        (lambda (num succ zero)
          (succ (num succ zero))))

       (+
        (lambda (num1 num2 succ zero)
          (num1 succ (num2 succ zero))))

       (*
        (lambda (num1 num2 succ zero)
          (num1 (num2 succ) zero)))

       (1 (inc 0))

       (2 (inc 1))

       (3 (inc 2))

       (4 (inc 3))

       (simple (make-cons 1 nil))

       (two (make-cons q (make-cons w nil)))

       (nums (make-cons 1 (make-cons 2 (make-cons 3 (make-cons 4 nil)))))

       (terms (make-cons q (make-cons z (make-cons x (make-cons c nil)))))

       (fold-right (lambda (trans init list) (list trans init)))

       (product (fold-right * 1)))

  nums)
