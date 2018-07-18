module Syntax.Scheme where

import Prelude hiding (read, succ)
import qualified Prelude (read)

import Term
import Syntax.Sexp


extract :: Sexp -> Term
extract sexp =
  let
    app_chain terms acc =
      case terms of
        [] ->
          acc

        term : rest ->
          app_chain rest (App acc (extract term))

    lambda_chain ids acc =
      case ids of
        [] ->
          acc

        (Symbol id) : rest ->
          lambda_chain rest (Abs id acc)
  in
  case sexp of
    Symbol id ->
      Var id

    List [(Symbol "lambda"), List args, body] ->
      let
        (Symbol id) : rest = reverse args
      in
        lambda_chain rest (Abs id (extract body))

    List (abs : term : rest) ->
      app_chain rest (App (extract abs) (extract term))
