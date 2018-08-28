module Term where

import Debug.Trace
import Data.IORef (newIORef, readIORef, modifyIORef)
import System.IO.Unsafe (unsafePerformIO)

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
          substq id right body
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
          substq id right body

        left' ->
          App (normal_order_once left') (normal_order_once right)


eta :: Term -> Term
eta term =
  case term of
    Abs x (App qwe (Var y)) | x == y ->
      qwe
    _ ->
      term


pass :: Term -> Term
pass term =
  normal_order_once term
--  eta (normal_order_once ()


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


is_free :: Id -> Term -> Bool
is_free name term =
  case term of
    Var id ->
      name == id

    Abs id body ->
      name /= id && is_free name body

    App left right ->
      is_free name left || is_free name right


ref = unsafePerformIO (newIORef 0)

new :: Id -> Id
new name =
  unsafePerformIO $ do
    modifyIORef ref (+1)
    c <- readIORef ref
    return (name <> show c)


substq :: Id -> Term -> Term -> Term
substq name new_term term =
  case term of
    Var id | id == name ->
      new_term

    Var _ ->
      term

    Abs id body | id == name ->
      term

    Abs id body | is_free id new_term && is_free name body ->
      let
        id' = new id
        body' = substq id (Var id') body
      in
        Abs id' (substq name new_term body')

    Abs id body ->
      Abs id (substq name new_term body)

    App left right ->
      App (substq name new_term left) (substq name new_term right)


cbn :: Term -> Term
cbn term =
  case term of
    Var id ->
      Var id

    Abs id body ->
      Abs id body

    App left right ->
      case cbn left of
        Abs id body ->
          cbn (substq id right body)

        left' ->
          App left' right


nor :: Term -> Term
nor term =
  case term of
    Var id ->
      Var id

    Abs id body ->
      Abs id (nor body)

    App left right ->
      case cbn left of
        Abs id body ->
          nor (substq id right body)

        left' ->
          App (nor left') (nor right)
