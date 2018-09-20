(let* ((true
        (lambda (true false)
          true))

       (false
        (lambda (true false)
          false))

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

       (0
        (lambda (succ zero)
          zero))

       (0?
        (lambda (num)
          (num (lambda (_any) false) true))))

  (if (not (0? false))
      zero-not-false
      wut?))
