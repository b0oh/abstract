module Term where

import Optional


type Id = String

data Term =
  Var Id |
  Abs Id Term |
  App Term Term deriving (Show, Eq)


is_free :: Id -> Term -> Bool
is_free name term =
  case term of
    -- The free variables of x are just x
    Var id ->
      id == name

    -- The set of free variables of (x -> t) is the set of free
    -- variables of t, but with x removed
    Abs id _body | id == name ->
      False

    Abs _id body ->
      is_free name body

    -- The set of free variables of (t s) is the union of the set of
    -- free variables of t and the set of free variables of s.
    App left right ->
      is_free name left || is_free name right


-- is_fresh :: Id -> Term -> Bool
-- is_fresh name term =


subst_ :: Id -> Term -> Term -> Term
subst_ name new_term term =
  case term of
    Var id | id == name ->
      new_term
    Var _id ->
      term
    App left right ->
      App (subst_ name new_term left) (subst_ name new_term right)
    Abs id body | id == name ->
      term
    Abs id body | not (is_free id new_term) ->
      Abs id (subst_ name new_term body)


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


alpha_ :: Term -> Term
alpha_ term =
  let
    iter :: Int -> [Id] -> Term -> Term
    iter depth names term =
      case term of
        Abs id body ->
          if id `elem` names then
            let
              new_id = id ++ "$" ++ show depth
              alphed_body = iter (depth + 1) (new_id : names) body
              new_body = subst id (Var new_id) alphed_body
            in
              Abs new_id new_body
          else
            Abs id (iter (depth + 1) (id : names) body)
        App left right ->
          App (iter (depth + 1) names left) right
        _ ->
          term
  in
    iter 0 [] term


alpha :: Term -> Term
alpha term =
  let
    iter term depth =
      case term of
        Var id ->
          (Var id, depth + 1)

        App left right ->
          let
            (left', depth') = iter left (depth + 1)
            (right', depth'') = iter right (depth' + 1)
          in
            (App left' right', depth'' + 1)

        Abs id body ->
          let
            (body', depth') = iter body (depth + 1)
            new_id = id ++ "$" ++ show depth'
            new_term = Var new_id
            body'' = subst id new_term body'
          in
            (Abs new_id body'', depth' + 1)

    (term', _) = iter term 0
  in
    term'



beta :: Term -> Term
beta term =
  case term of
    App (Abs id body) right ->
      subst id right body
    _ ->
      term


full_beta :: Term -> Term
full_beta term =
  case term of
    Var id ->
      Var id
    App (Abs id body) right ->
       subst id right body
    App left right ->
      App (full_beta left) (full_beta right)
    Abs id body ->
      Abs id (full_beta body)


reduce_ ::



reduce :: Term -> [Term]
reduce term =
  let
    step term acc =
      let
--        term' = lazy_alpha term
        term'' = full_beta term
      in
        if term'' == term then
          term : acc
        else
          step term'' (term'' : acc)
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
