module Syntax.Scheme where

import Prelude hiding (read, succ)
import qualified Prelude (read)

import Term
import Syntax.Sexp


extract :: Sexp -> Term
extract sexp =
  let
    app_iter terms acc =
      case terms of
        [] ->
          acc

        term : rest ->
          app_iter rest (App acc (extract term))

    make_lambda args body =
      let
        (Symbol id) : rest = args
        iter args acc =
          case args of
            [] ->
              acc

            (Symbol id) : rest ->
              iter rest (Abs id acc)
      in
        iter rest (Abs id (extract body))

    get_let_assignments assignments =
      let
        folder (List [name, value]) (names, values) = (name : names, value : values)
      in
        foldr folder ([], []) assignments

    let_iter values acc =
      case values of
        [] ->
          acc
        value : rest ->
          let_iter rest (App acc (extract value))

    let_star_to_let assignments body =
      let
        iter assignments acc =
          case assignments of
            [] ->
              acc
            List [name, value] : rest ->
              List [(Symbol "let"), List [List [name, value]], iter rest acc]
      in
        iter assignments body

  in
    case sexp of
      Symbol id ->
        Var id

      List [(Symbol "lambda"), List args, body] ->
        make_lambda (reverse args) body

      List [(Symbol "let"), List assignments, body] ->
        let
          (names, values) = get_let_assignments assignments
          let_body = make_lambda (reverse names) body
        in
          let_iter values let_body

      -- List [(Symbol "let*"), List assignments, body] ->
      --   let
      --     new_sexp = let_star_to_let assignments body
      --   in
      --     extract new_sexp

      List (abs : term : rest) ->
        app_iter rest (App (extract abs) (extract term))
