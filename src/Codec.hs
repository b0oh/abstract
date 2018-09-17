module Codec where

import Data.List (intercalate)

import Optional
import Term
import Show


data Decoded = Nil | Nat Int | Pair Decoded Decoded | List [Decoded] | Term Term


show_decoded :: Decoded -> String
show_decoded decoded =
  case decoded of
    Nil ->
      "#nil"

    Pair first second ->
      "(" ++ show_decoded first ++ ", " ++ show_decoded second ++ ")"

    List list ->
      "[" ++ intercalate ", " (map show_decoded list) ++ "]"

    Nat num ->
      "#" ++ show num

    Term term ->
      show_term term


some :: Optional a -> a -> a
some optional none =
  case optional of
    Some value ->
      value

    None ->
      none


decode :: Term -> Optional Decoded
decode term =
  case decode_nil term of
    Some result ->
      Some result

    None ->
      case decode_pair term of
        Some pair ->
          Some pair

        None ->
          case decode_list term of
            Some list ->
              Some list

            None ->
              case decode_nat term of
                Some nat ->
                  Some nat

                None ->
                  None


decode_nil :: Term -> Optional Decoded
decode_nil term =
  case term of
    Abs always (Abs never0 (Var never1)) | always /= never0 && never0 == never1 ->
      Some Nil

    _ ->
      None


decode_pair :: Term -> Optional Decoded
decode_pair term =
  case term of
    Abs pair0 (App (App (Var pair1) first) second) | pair0 == pair1 ->
      let
        first' = some (decode first) (Term first)
        second' = some (decode second) (Term second)
      in
        Some (Pair first' second')

    _ ->
      None


decode_list :: Term -> Optional Decoded
decode_list term =
  case term of
    Abs trans0 (Abs init0 elems) ->
       let
         iter acc term =
           case term of
             Var init1 | init0 == init1 ->
               Some (List (reverse acc))

             App (App (Var trans1) elem) rest | trans0 == trans1 ->
               iter (some (decode elem) (Term elem) : acc) rest

             _ ->
               None

       in
         iter [] elems

    _ ->
      None


decode_nat :: Term -> Optional Decoded
decode_nat value =
  case value of
    Abs succ (Abs zero1 (Var zero2)) | succ /= zero1 && zero1 == zero2 ->
      Some (Nat 0)
    Abs succ1 (Abs zero (App (Var succ2) body)) | succ1 == succ2 ->
      case decode_nat (Abs succ1 (Abs zero body)) of
        Some (Nat nat) ->
          Some (Nat (nat + 1))
        None ->
          None
    _ ->
      None
