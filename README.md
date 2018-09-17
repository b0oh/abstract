# Abstract

Programming language based on untyped lambda calculus

### Build

```
$ stack build
```

### Run

```
$ stack exec abstract examples/008-factorial.scm
λ > (identity → (const → (nil → (Y → (true → (false → (nil? → (and → (if → (0 → (inc → (+ → (* → (dec → (- → (0? → (leq? → (nat/eq? → (1 → (2 → (fac → fac ((+ 1) 2)) (Y (fac num → ((if ((nat/eq? num) 0)) 1) (* num) fac ((- num) 1)))) ((+ 1) 1)) (inc 0)) (num1 num2 → (and ((leq? num1) num2)) (leq? num2) num1)) (num1 num2 → 0? ((- num1) num2))) nil?) (num1 num2 → (num2 dec) num1)) (num succ zero → ((num (g h → h (g succ))) const zero) identity)) (num1 num2 succ zero → (num1 (num2 succ)) zero)) (num1 num2 succ zero → (num1 succ) (num2 succ) zero)) (num succ zero → succ ((num succ) zero))) nil) (pred true-clause false-clause → (pred true-clause) false-clause)) (pred1 pred2 → (pred1 pred2) pred1)) (term → (term (const false)) true)) nil) const) (g → (f → f f) (x → g (x x)))) (_always never → never)) (always _never → always)) (same → same)
λ > (succ zero → succ (succ (succ (succ (succ (succ zero))))))
Term detected: #6
```

Run `ghci`

```
$ stack ghci --main-is abstract:exe:scheme
> run_file "examples/008-factorial.scm"
```
