module Syntax.Calc where

import Prelude hiding (read, succ)
import qualified Prelude (read)

import Term
import Syntax.Sexp


succ =
  Abs "num"
    (Abs "succ"
      (Abs "zero"
        (App
          (Var "succ")
          (App
            (App (Var "num") (Var "succ"))
            (Var "zero")))))


plus =
  Abs "num1"
    (Abs "num2"
      (Abs "succ"
        (Abs "zero"
          (App
            (App (Var "num1") (Var "succ"))
            (App
              (App (Var "num2") (Var "succ"))
              (Var "zero"))))))


extract_nat :: Integer -> Term
extract_nat n =
  case n of
    0 ->
      Abs "succ" (Abs "zero" (Var "zero"))
    _ ->
      let
        Abs "succ" (Abs "zero" body) = extract_nat (n - 1)
      in
        Abs "succ" (Abs "zero" (App (Var "succ") body))


extract :: Sexp -> Term
extract sexp =
  case sexp of
    Symbol id ->
      case id of
        "+1" ->
          succ
        "+" ->
          plus
        nat ->
          extract_nat (Prelude.read nat)
    List [op, num] ->
      let
        op' = extract op
        num' = extract num
      in
        App op' num'
    List [op, num1, num2] ->
      let
        op' = extract op
        num1' = extract num1
        num2' = extract num2
      in
        App (App op' num1') num2'
