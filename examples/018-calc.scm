(let* (;; copy of our small library

       (identity
        (lambda (same)
          same))

       (const
        (lambda (always _never)
          always))

       (nil
        (lambda (_always never)
          never))

       (self-rec
        (lambda (self)
          (let ((f (lambda (x) (self (x x)))))
            (f f))))

       (|>
        (lambda (fun1 fun2 arg)
          (fun2 (fun1 arg))))

       ;; logic

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

       ;; pairs

       (pair/make
        (lambda (first second pair)
          (pair first second)))

       (first
        (lambda (pair)
          (pair const)))

       (second
        (lambda (pair)
          (pair nil)))

       ;; natural numbers

       (inc
        (lambda (num succ zero)
          (succ (num succ zero))))

       (+
        (lambda (num1 num2 succ zero)
          (num1 succ (num2 succ zero))))

       (*
        (lambda (num1 num2 succ zero)
          (num1 (num2 succ) zero)))

       (^
        (lambda (num1 num2)
          (num2 num1)))

       (dec
        (lambda (num succ zero)
          (num (lambda (g h) (h (g succ)))
               (const zero)
               identity)))

       (-
        (lambda (num1 num2)
          (num2 dec num1)))

       (0? nil?)

       (nat/leq?
        (lambda (num1 num2)
          (0? (- num1 num2))))

       (nat/eq?
        (lambda (num1 num2)
          (and (nat/leq? num1 num2) (nat/leq? num2 num1))))

       ;; lists

       (cons
        (lambda (head tail trans init)
          (trans head (tail trans init))))

       (head
        (lambda (elems)
          (elems const nil)))

       (tail
        (lambda (elems cons init)
          (let ((helper
                 (lambda (head tail trans)
                   (trans head (tail cons)))))

            (elems helper (const init) nil))))

       (fold-right
        (lambda (trans init elems)
          (elems trans init)))

       (empty? (fold-right (lambda (_head _tail) false) true))

       (map
        (lambda (fun)
          (fold-right (|> fun cons) nil)))

       (append
        (lambda (xs ys)
          (fold-right cons ys xs)))

       (concat (fold-right append nil))


       ;; strings

       (string/eq?
        (lambda (string1 string2)
          (let ((iter
                 (self-rec (lambda (iter chars1 chars2)
                             (if (or
                                  (and (empty? chars1) (not (empty? chars2)))
                                  (and (not (empty? chars1)) (empty? chars2)))
                                 false
                                 (if (and (empty? chars1) (empty? chars2))
                                     true
                                     (and
                                      (nat/eq? (head chars1) (head chars2))
                                      (iter (tail chars1) (tail chars2)))))))))

          (iter string1 string2))))

       ;; calc

       (id/eq? string/eq?)

       (id/show identity)

       (tag/eq? nat/eq?)

       (tag-with pair/make)

       (untag second)

       (tagged-with?
        (lambda (tag term)
          (tag/eq? (first term) tag)))


       (var/tag #1)

       (var? (tagged-with? var/tag))

       (var/make (tag-with var/tag))

       (var/id untag)


       (abs/tag #2)

       (abs? (tagged-with? abs/tag))

       (abs/make
        (lambda (id body)
          (tag-with abs/tag (pair/make id body))))

       (abs/id (|> untag first))

       (abs/body (|> untag second))


       (app/tag #3)

       (app? (tagged-with? app/tag))

       (app/make
        (lambda (abs arg)
          (tag-with app/tag (pair/make abs arg))))

       (app/abs (|> untag first))

       (app/arg (|> untag second))


       (term/fold
        (lambda (var-folder abs-folder app-folder term)
          (if (var? term)
              (var-folder (var/id term))
              (if (abs? term)
                  (abs-folder (abs/id term) (abs/body term))
                  (if (app? term)
                      (app-folder (app/abs term) (app/arg term))
                      nonsense)))))

       (term/show
        (self-rec (lambda (term/show term)
             (let* ((space (list #\space))

                    (show-abs-chain
                     (self-rec (lambda (show-abs-chain term)
                                 (if (abs? term)
                                     (concat (list
                                              (id/show (abs/id term))
                                              space
                                              (show-abs-chain (abs/body term))))
                                     (concat (list
                                              (list #\- #\> #\space)
                                              (term/show term)))))))

                    (var id/show)

                    (abs
                     (lambda (_id _body)
                       (concat (list
                                (list #\()
                                (show-abs-chain term)
                                (list #\))))))

                    (app
                     (lambda (abs arg)
                       (concat (list
                                (term/show abs)
                                space
                                (term/show arg))))))

               (term/fold var abs app term)))))

       (beta
        (self-rec (lambda (beta name subst term)
                    (let ((var
                           (lambda (id)
                             (if (id/eq? id name)
                                 subst
                                 term)))

                          (abs
                           (lambda (id body)
                             (if (id/eq? id name)
                                 term
                                 (abs/make id (beta name subst body)))))

                          (app
                           (lambda (abs arg)
                             (app/make
                              (beta name subst abs)
                              (beta name subst arg)))))

                      (term/fold var abs app term)))))

       (reduce-once
        (self-rec (lambda (reduce-once term)
                    (let ((var (const term))

                          (abs
                           (lambda (id body)
                             (abs/make id (reduce-once body))))

                          (app
                           (lambda (abs arg)
                             (if (abs? abs)
                                 (let ((id (abs/id abs))
                                       (body (abs/body abs)))
                                   (beta id arg body))
                                 (app/make (reduce-once abs) (reduce-once arg))))))

                      (term/fold var abs app term)))))

       (normal?
        (self-rec (lambda (normal? term)
                    (let ((var (const true))

                          (abs
                           (lambda (_ body)
                             (normal? body)))

                          (app
                           (lambda (abs arg)
                             (if (abs? abs)
                                 false
                                 (and
                                  (normal? abs)
                                  (normal? arg))))))

                      (term/fold var abs app term)))))

       (full-beta
        (self-rec (lambda (full-beta term)
                    (let ((reduced (reduce-once term)))
                      (if (normal? reduced)
                          reduced
                          (full-beta redused)))))))
  (let* ((succ-id (list #\s #\u #\c #\c))
         (zero-id (list #\z #\e #\r #\o))
         (num-id (list #\n #\u #\m))
         ;; two = succ zero -> succ (succ zero)
         (two (abs/make succ-id
                        (abs/make zero-id
                                  (app/make
                                   (var/make succ-id)
                                   (app/make
                                    (var/make succ-id)
                                    (var/make zero-id))))))
         ;; +1 = num succ zero -> succ (num succ zero)
         (+1 (abs/make num-id
                       (abs/make succ-id
                                 (abs/make zero-id
                                           (app/make
                                            (var/make succ-id)
                                            (app/make
                                             (app/make
                                              (var/make num-id)
                                              (var/make succ-id))
                                             (var/make zero-id)))))))

         (nl (list #\nl))
         (term (app/make +1 two))
         (reduced (reduce-once term)))
    ;; (concat (list
    ;;          nl
    ;;          (term/show term)
    ;;          nl
    ;;          (term/show reduced)
    ;;          nl))))
    ;;    (beta num-id two +1)))
    ;; (if (normal? reduced)
    ;;     nonono
    ;;     yeyeyeye)))
    ;; (if (normal? term)
    ;;     pipiu
    ;;     qweqwe)))
reduced))
