module Term where

import Data.Char (isDigit)
import Data.List (union, dropWhileEnd)

import Optional

type Id = String

data Term =
  Var Id |
  Abs Id Term |
  App Term Term deriving (Show, Eq)



is_free :: Id -> Term -> Bool
is_free name term =
  case term of
    Var id ->
      name == id

    Abs id body ->
      name /= id && is_free name body

    App abs arg ->
      is_free name abs || is_free name arg


free_vars :: Term -> [Id]
free_vars term =
  let
    iter names term =
      case term of
        Var id | id `elem` names ->
          []

        Var id ->
          [id]

        Abs id body ->
          iter (id : names) body

        App abs arg ->
          iter names abs `union` iter names arg
  in
    iter [] term


make_new_id :: Id -> [Id] -> Id
make_new_id name names =
  let
    striped = dropWhileEnd isDigit name
    numbered = map (\num -> striped ++ show num) [1..]
    filtered = filter (`notElem` names) numbered

  in
    head filtered


alpha :: Id -> Id -> Term -> Term
alpha name new_name term =
  case term of
    Var id | id == name ->
      Var new_name

    Var _ ->
      term

    Abs id _ | id == name ->
      term

    Abs id body ->
      Abs id (alpha name new_name body)

    App abs arg ->
      App (alpha name new_name abs) (alpha name new_name arg)


simple_beta :: Id -> Term -> Term -> Term
simple_beta name new_term term =
  case term of
    Var id | id == name ->
      new_term

    Var _ ->
      term

    Abs id _ | id == name ->
      term

    Abs id body ->
      Abs id (simple_beta name new_term body)

    App abs arg ->
      App (simple_beta name new_term abs) (simple_beta name new_term arg)


beta :: Id -> Term -> Term -> Term
beta name new_term term =
  case term of
    Var id | id == name ->
      new_term

    Var _ ->
      term

    Abs id _ | id == name ->
      term

    Abs id body | is_free id new_term && is_free name body ->
      let
        new_term_free_vars = free_vars new_term
        body_free_vars = free_vars body
        free = new_term_free_vars `union` body_free_vars
        new_id = make_new_id id free

      in
        Abs new_id (beta name new_term (alpha id new_id body))

    Abs id body ->
      Abs id (beta name new_term body)

    App abs arg ->
      App (beta name new_term abs) (beta name new_term arg)


reduce_once :: Term -> Term
reduce_once term =
  case term of
    Var _ ->
      term

    Abs id body ->
      Abs id (reduce_once body)

    App (Abs id body) arg ->
      simple_beta id arg body

    App abs arg ->
      App (reduce_once abs) (reduce_once arg)


is_normal :: Term -> Bool
is_normal term =
  case term of
    Var _ ->
      True

    Abs _ body ->
      is_normal body

    App (Abs _ _) arg ->
      False

    App abs arg ->
      is_normal abs && is_normal arg


full_beta :: Term -> Term
full_beta term =
  let
    term' = reduce_once term

  in
    if is_normal term' then
      term'
    else
      full_beta term'


call_by_name :: Term -> Term
call_by_name term =
  case term of
    Var _ ->
      term

    Abs id body ->
      Abs id body

    App abs arg ->
      case call_by_name abs of
        Abs id body ->
          call_by_name (beta id arg body)

        abs' ->
          App abs' arg


normal_order :: Term -> Term
normal_order term =
  case term of
    Var _ ->
      term

    Abs id body ->
      Abs id (normal_order body)

    App abs arg ->
      case call_by_name abs of
        Abs id body ->
          normal_order (beta id arg body)

        abs' ->
          App (normal_order abs') (normal_order arg)


decode_nat :: Term -> Optional Integer
decode_nat value =
  case value of
    Abs succ (Abs zero1 (Var zero2)) | succ /= zero1 && zero1 == zero2 ->
      Some 0
    Abs succ1 (Abs zero (App (Var succ2) body)) | succ1 == succ2 ->
      case decode_nat (Abs succ1 (Abs zero body)) of
        Some nat ->
          Some (nat + 1)
        None ->
          None
    _ ->
      None
