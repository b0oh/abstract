(let* ((....))
  ;;; receive : channel0 -> pair term channel1
  ;;; send : channel0 -> term -> channel1
  (lambda (receive send channel)
    (let ((loop (Y (lambda (loop channel0))
                   (let* ((result (receive channel0))
                          (sexp (first result))
                          (channel1 (second result))
                          (term (extract sexp))
                          (normal (reduce normal-order term))
                          ;;; late binding how? probs using eval and environemnts
                          ;;; (expr (eval (make-env (list "receive" recieve "send" send "channel" channel1))))
                          (channel2 (send term #| or expr |# channel1)))
                     (loop channel2)))))
      (loop channel))))
