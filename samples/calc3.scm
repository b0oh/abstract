(* (+ (+1 0) (+1 0)) (+ (+1 0) (+1 0)))

((lambda (0)
   ((lambda (+1)
      ((lambda (1)
         (+1 1))
       (+1 0)))
    (lambda (num succ zero) (succ (num succ zero)))))
 (lambda (succ zero) zero))

(((lambda (0 +1) ((lambda (1) (+1 1))
                  (+1 0)))
 (lambda (succ zero) zero))
(lambda (num succ zero) (succ (num succ zero))))

(((lambda (0 +1) ((lambda (1) (+1 1))
                  (+1 0)))
 (lambda (succ zero) zero))
(lambda (num succ zero) (succ (num succ zero))))
