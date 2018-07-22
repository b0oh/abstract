((lambda (+1)
  ((lambda (0) (+1 0))
   (lambda (succ zero) zero)))
 (lambda (num succ zero) (succ (num succ zero))))


((((lambda (1 0 +1) 1)
   (lambda (num succ zero) (succ (num succ zero))))
  (lambda (succ zero) zero))
 (lambda (+1 0) (+1 0)))

(lambda (1) (lambda (0) (lambda ) +1) 1)

((((lambda (1+ 0 1) 1)
   (+1 0))
  (lambda (succ zero) zero))
 (lambda (num succ zero) (succ (num succ zero))))

(let ((+1 (lambda (num succ zero) (succ (num succ zero))))
     (0 (lambda (succ zero) zero))
     (1 (+1 0)))
 1)
