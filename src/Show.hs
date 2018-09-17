module Show where

import Term

show_term :: Term -> String
show_term term =
  let
    is_var term =
      case term of
        Var _ -> True
        _ -> False

    is_abs term =
      case term of
        Abs _ _ -> True
        _ -> False

    is_app term =
      case term of
        App _ _ -> True
        _ -> False

    show_abs term =
      case term of
        Abs id body ->
          id ++ " " ++ show_abs body
        _ ->
          "→ " ++ show_term term
  in
    case term of
      Var id ->
        id

      abs@(Abs _ _) ->
        "(" ++ show_abs abs ++ ")"

      App left right ->
        case () of
          _ | is_app left ->
            "(" ++ show_term left ++ ") " ++ show_term right
          _ | is_app right ->
            show_term left ++ " (" ++ show_term right ++ ")"
          -- _ | is_var right ->
          --
          -- _ | is_app right ->
          --   show_term left ++ " (" ++ show_term right ++ ")"
          -- _ | is_abs left ->
          --   "(" ++ show_term left ++ ") " ++ show_term right
          _ ->
            show_term left ++ " " ++ show_term right
