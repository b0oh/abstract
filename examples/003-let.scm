(let ((1 (lambda (succ zero) (succ zero)))
      (inc (lambda (num succ zero) (succ (num succ zero)))))
  (inc 1))
