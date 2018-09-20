module Term where

import Data.Char (isDigit)
import Data.List (union, dropWhileEnd)

type Id = String

data Term =
  Var Id |
  Abs Id Term |
  App Term Term deriving (Show, Eq)


beta :: Id -> Term -> Term -> Term
beta name subts term =
  case term of
    Var id | id == name ->
      subts

    Var _ ->
      term

    Abs id _ | id == name ->
      term

    Abs id body ->
      Abs id (beta name subts body)

    App abs arg ->
      App (beta name subts abs) (beta name subts arg)


reduce_once :: Term -> Term
reduce_once term =
  case term of
    Var _ ->
      term

    Abs id body ->
      Abs id (reduce_once body)

    App (Abs id body) arg ->
      beta id arg body

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


alpha_beta :: Id -> Term -> Term -> Term
alpha_beta name subst term =
  case term of
    Var id | id == name ->
      subst

    Var _ ->
      term

    Abs id _ | id == name ->
      term

    Abs id body | is_free id subst && is_free name body ->
      let
        subst_free_vars = free_vars subst
        body_free_vars = free_vars body
        free = subst_free_vars `union` body_free_vars
        new_id = make_new_id id free

      in
        Abs new_id (alpha_beta name subst (alpha id new_id body))

    Abs id body ->
      Abs id (alpha_beta name subst body)

    App abs arg ->
      App (alpha_beta name subst abs) (alpha_beta name subst arg)


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
          call_by_name (alpha_beta id arg body)

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
          normal_order (alpha_beta id arg body)

        abs' ->
          App (normal_order abs') (normal_order arg)
