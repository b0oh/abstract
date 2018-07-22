(let ((0 (lambda (succ zero) zero))
      (+1 (lambda (num succ zero) (succ (num succ zero))))
      (+ (lambda (num1 num2 succ zero) (num1 succ (num2 succ zero)))))
  (+ (+ (+1 0)
        (+1 0))
     (+ (+1 0)
        (+1 0))))

  (let ((1 (+1 0)))
    (+ (+ 1 1) (+ 1 1))))



    (let ((2 (+ 1 1)))
      (+ 2 2))))
