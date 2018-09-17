(let* ((const
        (lambda (always _never)
          always))

       (nil
        (lambda (_always never)
          never))

       (pair/make
        (lambda (first second pair)
          (pair first second)))

       (first
        (lambda (pair)
          (pair const)))

       (second
        (lambda (pair)
          (pair nil)))

       (inc
        (lambda (num succ zero)
          (succ (num succ zero))))

       (0 nil)

       (1 (inc nil)))
  (pair/make nil (inc (inc 1))))
