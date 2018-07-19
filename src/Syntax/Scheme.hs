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

    let_chain values acc =
      case values of
        [] ->
          acc
        value : rest ->
          let_chain rest (App acc (extract value))

  in
    case sexp of
      Symbol id ->
        Var id

      List [(Symbol "lambda"), List args, body] ->
        let
          (Symbol id) : rest = reverse args
        in
          lambda_chain rest (Abs id (extract body))

      List [(Symbol "let"), List assignments, body] ->
        let
          folder (List [name, value]) (names, values) = (name : names, value : values)
          (names, values) = foldr folder ([], []) assignments
          (Symbol id) : rest = reverse names
          let_body = lambda_chain rest (Abs id (extract body))
        in
          let_chain values let_body

      List (abs : term : rest) ->
        app_chain rest (App (extract abs) (extract term))
