(let* ((pair/make
        (lambda (first second pair)
          (pair first second)))

       (first
        (lambda (pair)
          (pair (lambda (first second) first))))

       (second
        (lambda (pair)
          (pair (lambda (first second) second))))

       (+
        (lambda (pair succ zero)
          ((first pair) succ ((second pair) succ zero))))

       (1 (lambda (succ zero) (succ zero)))

       (2 (lambda (succ zero) (succ (succ zero))))

       (some-pair (pair/make 1 2)))

  (+ some-pair))
