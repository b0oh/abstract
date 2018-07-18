# Abstract

Programming language based on untyped lambda calculus

### Build

```
$ stack build
```

### Run

Run _Calc.hs_ module

```
$ stack exec calc samples/calc2.scm
λ > num1 num2 succ zero → num1 succ (num2 succ zero) (num succ zero → succ (num succ zero) (succ zero → succ (succ zero))) (succ zero → succ (succ (succ zero)))
α > $2 $3 $4 $5 → $2 $4 ($3 $4 $5) ($16 $17 $18 → $17 ($16 $17 $18) ($26 $27 → $26 ($26 $27))) ($33 $34 → $33 ($33 ($33 $34)))
β > $3 $4 $5 → $16 $17 $18 → $17 ($16 $17 $18) ($26 $27 → $26 ($26 $27)) $4 ($3 $4 $5) ($33 $34 → $33 ($33 ($33 $34)))
β > $4 $5 → $16 $17 $18 → $17 ($16 $17 $18) ($26 $27 → $26 ($26 $27)) $4 (($33 $34 → $33 ($33 ($33 $34))) $4 $5)
β > $4 $5 → ($17 $18 → $17 (($26 $27 → $26 ($26 $27)) $17 $18)) $4 (($34 → $4 ($4 ($4 $34))) $5)
β > $4 $5 → $18 → $4 (($26 $27 → $26 ($26 $27)) $4 $18) ($4 ($4 ($4 $5)))
β > $4 $5 → $4 (($26 $27 → $26 ($26 $27)) $4 ($4 ($4 ($4 $5))))
β > $4 $5 → $4 ($27 → $4 ($4 $27) ($4 ($4 ($4 $5))))
β > $4 $5 → $4 ($4 ($4 ($4 ($4 ($4 $5)))))
Natural number detected: 6
```

Run _Scheme.hs_ module

```
$ stack exec scheme samples/scheme0.scm
λ > 1 +1 → +1 1 (succ zero → succ zero) (num succ zero → succ (num succ zero))
α > $2 $3 → $3 $2 ($7 $8 → $7 $8) ($12 $13 $14 → $13 ($12 $13 $14))
β > $3 → $3 ($7 $8 → $7 $8) ($12 $13 $14 → $13 ($12 $13 $14))
β > $12 $13 $14 → $13 ($12 $13 $14) ($7 $8 → $7 $8)
β > $13 $14 → $13 (($7 $8 → $7 $8) $13 $14)
β > $13 $14 → $13 (($8 → $13 $8) $14)
β > $13 $14 → $13 ($13 $14)
Natural number detected: 2
```
