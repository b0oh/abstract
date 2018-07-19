(let ((1 (lambda (succ zero) (succ zero)))
      (+1 (lambda (num succ zero) (succ (num succ zero)))))
  (+1 1))
