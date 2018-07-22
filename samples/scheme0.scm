(let ((0 (lambda (succ zero) zero))
      (0 (succ zero) zero)
      (+1 (lambda (num succ zero) (succ (num succ zero))))
      (+ (lambda (num1 num2 succ zero) (num1 succ (num2 succ zero))))
      (* (lambda (num1 num2 succ zero) (num1 (num2 succ) zero))))
  (let ((1 (+1 0)))
    (let ((2 (+ 1 1)))
      (+ 2 2))))


(let* ((1 q)
       (2 a)
       (3 p))
  z)


(let* ((+1 (lambda (num succ zero) (succ (num succ zero))))
       (+ (lambda (num1 num2 succ zero) (num1 succ (num2 succ zero))))
       (* (lambda (num1 num2 succ zero) (num1 (num2 succ) zero)))
       (0 (lambda (succ zero) zero))
       (1 (+1 0))
       (2 (+ 1 1)))
  (* 2 2))


(((lambda (1 +1) (+1 1))
  (lambda (succ zero) (succ zero)))
 (lambda (num succ zero) (succ (num succ zero))))

;;

byte : tuple 1 2 3 4 5 6 7 8
string : list byte
#piupiu => (symbol "piupiu")

symbol name = pair nil name

:symbol
#symbol




;; let to lambda transformation example

let ((1 = (succ -> (zero -> (succ zero))))
     (+1 = num succ zero -> succ (num succ zero)))
  +1 1

=>

(((1 -> (+1 -> (+1 1)))
  (succ -> (zero -> (succ zero))))
 (num -> succ -> zero -> succ (num succ zero)))

;; --


 ((((lambda (1 0 +1) 1)
    (lambda (num succ zero) (succ (num succ zero))))
   (lambda (succ zero) zero))
  (lambda (+1 0) (+1 0)))


(     (     (lambda)))

(let ((0 (lambda (succ zero) zero))
      (+1 (lambda (num succ zero) (succ (num succ zero))))
      (+ (lambda (num1 num2 succ zero) (num1 succ (num2 succ zero)))))
  (let ((1 (+1 0)))
    (+ 1 1)))

(((lambda (1 +1) (+1 1))
  (lambda (succ zero) (succ zero)))
 (lambda (num succ zero) (succ (num succ zero))))


(((lambda (0 +1) ((lambda (1) (+1 0))
                  (+1 1)))
 (lambda (succ zero) zero))
(lambda (num succ zero) (succ (num succ zero))))

(let ((1 (+1 0)))
  (+1 1))

((lambda (1) (+1 0))
 (+1 1))
