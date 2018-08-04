module TermSpec where

import Test.Hspec
import Control.Exception (evaluate, PatternMatchFail(..))

import Term
import Syntax.Sexp
import Syntax.Scheme


matchFailException :: Selector PatternMatchFail
matchFailException = const True


read_extract = extract . read_


spec :: Spec
spec = do
  describe "extract" $ do
    it "extracts free variable" $ do
      let term = read_extract "x"
      term `shouldBe` Var "x"

    it "extracts abstraction" $ do
      let term = read_extract "(lambda (x) x)"
      term `shouldBe` Abs "x" (Var "x")

    it "extracts application" $ do
      let term = read_extract "((lambda (x) x) y)"
      term `shouldBe` App (Abs "x" (Var "x"))
                          (Var "y")

    it "extracts nested abstractions" $ do
      let term = read_extract "((lambda (x) (lambda (x) x)) y)"
      term `shouldBe` App (Abs "x" (Abs "x" (Var "x")))
                          (Var "y")

    it "extracts nested application" $ do
      let term = read_extract "((lambda (x) ((lambda (x) x) x)) y)"
      term `shouldBe` App (Abs "x" (App (Abs "x" (Var "x"))
                                        (Var "x")))
                          (Var "y")

  describe "is_free" $ do
    it "TODO variable" $ do
      is_free "x" (Var "x") `shouldBe` True
      is_free "y" (Var "x") `shouldBe` False

    it "TODO abstraction" $ do
      is_free "x" (Abs "x" (Var "x")) `shouldBe` False
      is_free "x" (Abs "x" (Var "y")) `shouldBe` False
      is_free "x" (Abs "y" (Var "x")) `shouldBe` True

    it "TODO application" $ do
      is_free "x" (App (Var "x") (Var "y")) `shouldBe` True
      is_free "x" (App (Var "y") (Var "x")) `shouldBe` True
      is_free "x" (App (Var "y") (Var "z")) `shouldBe` False

  describe "capture avoiding substitution" $ do
    it "substitutes bound variable" $ do
      -- x[x := r] = r
      subst_ "x" (Var "r") (Var "x") `shouldBe` (Var "r")

    it "ignores when variable name doesn't match" $ do
      -- y[x := r] = y
      subst_ "x" (Var "r") (Var "y") `shouldBe` (Var "y")

    it "ignores when application terms has no matching variables" $ do
      -- (ts)[x := r] = (t[x := r])(s[x := r])
      subst_ "x" (Var "r") (App (Var "t") (Var "s")) `shouldBe` (App (Var "t") (Var "s"))

    it "recursivly substitues bound variables in application" $ do
      -- (ts)[x := r] = (t[x := r])(s[x := r])
      subst_ "x" (Var "r") (App (Var "x") (Var "s")) `shouldBe` (App (Var "r") (Var "s"))

    it "ignores when variable is bound in abstraction" $ do
      -- (x -> t)[x := r] = (x -> t)
      subst_ "x" (Var "r") (Abs "x" (Var "t")) `shouldBe` (Abs "x" (Var "t"))

    it "qwe" $ do
      -- (y -> t)[x := r] = (y -> t[x := r]) if x =/= y
      subst_ "x" (Var "r") (Abs "y" (Var "t")) `shouldBe` (Abs "y" (Var "t"))
      subst_ "x" (Var "r") (Abs "y" (Var "x")) `shouldBe` (Abs "y" (Var "r"))

    it "fails when substitute is bound in new term" $ do
      evaluate (subst_ "x" (Var "y") (Abs "y" (Var "t"))) `shouldThrow` matchFailException

  describe "alpha" $ do
    it "zxc" $ do
      alpha_ (Abs "x" (Var "x")) `shouldBe` (Abs "x" (Var "x"))
      alpha_ (Abs "x" (Abs "x" (Var "x"))) `shouldBe` (Abs "x" (Abs "x$1" (Var "x$1")))

  describe "reduce" $ do
    it "zxcv" $ do
      let zero = Abs "succ" (Abs "zero" (Var "zero"))
      let inc = Abs "num" (Abs "succ" (Abs "zero" (App (Var "succ") (App (App (Var "num") (Var "succ")) (Var "zero")))))
      pass (pass (App inc zero)) `shouldBe` (Abs "succ" (Abs "zero" (App (Var "succ") (Var "zero"))))

    it "poipo" $ do
      let term = read_extract "((lambda (0)\
                              \   ((lambda (+1)\
                              \      ((lambda (1)\
                              \         (+1 1))\
                              \       (+1 0)))\
                              \    (lambda (num succ zero) (succ (num succ zero)))))\
                              \ (lambda (succ zero) zero))"

      pass (pass (pass (pass (pass (pass term))))) `shouldBe` Abs "succ" (Abs "zero" (App (Var "succ") (App (Var "succ") (Var "zero"))))

    it "fkn hell" $ do
      contents <- readFile "samples/2-nested-let.scm"
      let term = read_extract contents
      term `shouldBe` Var "q"

      -- (x ((x -> x) y))
      -- (x -> (y -> x y) (x -> x))
      -- (x -> (x -> x) (y -> x y))




    -- it "substitues bound variables according to their scope" $ do
    --   let term = read_extract "((lambda (x) ((lambda (x) x) x)) y)"

    --   "(x -> (x' -> x') x) y"
    --   let term' = alpha term
    --   term' `shouldBe` App (Abs "x$5" (App (Abs "x$4" (Var "x$4"))
    --                                        (Var "x$5")))
    --                        (Var "y")
