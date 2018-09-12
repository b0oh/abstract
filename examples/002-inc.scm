(((lambda (1 inc) (inc 1))
  (lambda (succ zero) (succ zero)))
 (lambda (num succ zero) (succ (num succ zero))))
