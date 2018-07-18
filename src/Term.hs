module Term where

import Optional


type Id = String

data Term =
  Var Id |
  Abs Id Term |
  App Term Term deriving (Show, Eq)


subst :: Id -> Term -> Term -> Term
subst seek sub term =
  case term of
    App left right ->
      App (subst seek sub left) (subst seek sub right)
    Abs id body ->
      Abs id (subst seek sub body)
    Var id | id == seek ->
      sub
    Var id ->
      Var id


alpha :: Integer -> Term -> (Term, Integer)
alpha depth term =
  case term of
    Var id ->
      (Var id, depth)

    App left right ->
      let
        (left', depth') = alpha (depth + 1) left
        (right', depth'') = alpha (depth' + 1) right
      in
        (App left' right', depth'')

    Abs id body ->
      let
        id' = "$" ++ show depth
        body' = subst id (Var id') body
        (body'', depth') = alpha (depth + 1) body'
      in
        (Abs id' body'', depth')


beta :: Term -> Term
beta term =
  case term of
    Var id ->
      Var id
    App (Abs id body) right ->
      subst id right body
    App left right ->
      App (beta left) (beta right)
    Abs id body ->
      Abs id (beta body)


beta_full :: Term -> [Term]
beta_full term =
  let
    step term acc =
      let
        term' = beta term
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
