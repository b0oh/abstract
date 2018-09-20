(let* ((identity
        (lambda (same)
          same))

       (const
        (lambda (always _never)
          always))

       (nil
        (lambda (_always never)
          never))

       (true const)

       (false nil)

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

       (nil?
        (lambda (term)
          (term (const false) true)))

       (pair/make
        (lambda (first second pair)
          (pair first second)))

       (first
        (lambda (pair)
          (pair const)))

       (second
        (lambda (pair)
          (pair nil)))

       (0 nil))

  (if (and (and (nil? nil) (nil? false)) (nil? 0))
      everything-is-a-nil
      or?))
