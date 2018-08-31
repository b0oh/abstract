(let* ((fold-term
        (lambda (var-folder abs-folder app-folder term)
          (cond ((var? term)
                 (var-folder (var/get-id term)))
                ((abs? term)
                 (abs-folder (abs/get-id term) (abs/get-body term)))
                ((app? term)
                 (app-folder (app/get-left term) (app/get-right term))))))

       (free?
        (lambda (name term)
          (let ((var
                 (lambda (id)
                   (identifier/eq? id name)))

                (abs
                 (lambda (id body)
                   (and (not (identifier/eq? id name))
                        (free? name body))))

                (app
                 (lambda (left right)
                   (or (free? name left)
                       (free? name right)))))

            (fold-term var abs app term))))

       (subst
        (lambda (name new-term term)
          (let ((var
                 (lambda (id)
                   (if (identifier/eq? id name)
                       new-term
                       term)))

                (abs
                 (lambda (id body)
                   (cond ((identifier/eq? id name)
                          term)

                         ((and (free? id new-term)
                               (free? name body))
                          (let ((id' (new id))
                                (body' (subst name new-term body)))
                            (abs/make id' (subst name name-term body'))))

                         (else
                          (abs/make id (subst name new-term body))))))

                (app
                 (lambda (left right)
                   (app/make
                    (subst name new-term left)
                    (subst name new-term right)))))

            (fold-term var abs app term))))

       (call-by-name
        (lambda (term)
          (let ((var (lambda (_) term))

                (abs (lambda (_ _) term))

                (app
                 (lambda (left right)
                   (let ((left' (call-by-name left)))
                     (if (abs? left')
                         (let ((id (abs/get-id left'))
                               (body (abs/get-body left')))
                           (call-by-name (subst id right body)))
                         (app/make left' right))))))

            (fold-term var abs app term))))


       (normal-order
        (lambda (term)
          (let ((var (lambda (_) term))

                (abs
                 (lambda (id body)
                   (abs/make id (normal-order body))))

                (app
                 (lambda (left right)
                   (let ((left' (call-by-name left)))

                     (if (abs? left')
                         (let ((id (abs/get-id left'))
                               (body (abs/get-body left')))

                           (normal-order (subst id right body)))

                         (app/make (normal-order left') (normal-order right)))))))

            (fold-term var abs app term)))))

  (repl))
