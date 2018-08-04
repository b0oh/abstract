module Term where

import Debug.Trace

import Optional

type Id = String

data Term =
  Var Id |
  Abs Id Term |
  App Term Term deriving (Show, Eq)


subst :: Id -> Term -> Term -> Term
subst name new_term term =
  case term of
    App left right ->
      App (subst name new_term left) (subst name new_term right)
    Abs id body ->
      Abs id (subst name new_term body)
    Var id | id == name ->
      new_term
    Var id ->
      term


alpha :: Term -> Term
alpha term =
  let
    iter :: Int -> [Id] -> Term -> (Term, Int, [Id])
    iter depth names term =
      case term of
        Abs id body ->
          if id `elem` names then
            let
              new_id = id ++ "$" ++ show depth
              (alphed_body, depth', names') = iter (depth + 1) (new_id : names) body
              new_body = subst id (Var new_id) alphed_body
            in
              (Abs new_id new_body, depth' + 1, names')
          else
            let
              (body', depth', names') = iter (depth + 1) (id : names) body
            in
              (Abs id body', depth' + 1, names')
        App left right ->
          let
            (left', depth', names') = (iter (depth + 1) names left)
            (right', depth'', names'') = (iter (depth' + 1) names' right)
          in
            (App left' right', depth'', names'')
        _ ->
          (term, depth + 1, names)

    (term', _, _) = iter 0 [] term
  in
    term'


call_by_name_once :: Term -> Term
call_by_name_once term =
  case term of
    Var _ ->
      term
    Abs id body ->
      term
    App left right ->
      case call_by_name_once left of
        Abs id body ->
          subst id right body
        left' ->
          App left' right


normal_order_once :: Term -> Term
-- normal_order_once term | trace ("once: " ++ show term) False = undefined
normal_order_once term =
  case term of
    Var _ ->
      term
    Abs id body ->
      Abs id (normal_order_once body)
    App left right ->
      case call_by_name_once left of
        Abs id body ->
          subst id right body
        left' ->
          let
            left'' = normal_order_once left'
          in
            App left'' (normal_order_once right)


pass :: Term -> Term
pass term =
  normal_order_once (alpha term)


reduce :: Term -> [Term]
reduce term =
  let
    step term acc =
      let
        term' = pass term
      in
        if term' == term then
          term : acc
        else
          step term' (term' : acc)

  in
    step term []


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
