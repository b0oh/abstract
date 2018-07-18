module Main where

import System.Environment

import Optional
import Term
import Show
import Syntax.Sexp
import Syntax.Scheme


print_steps :: [Term] -> IO ()
print_steps steps =
  case steps of
    [] ->
      pure ()
    term : rest ->
      do
        print_steps rest
        putStrLn ("β > " ++ show_term term)


run_file :: String -> IO ()
run_file file_name = do
  contents <- readFile file_name
  let sexp = read_ contents
  let term = extract sexp
  putStrLn ("λ > " ++ show_term term)
  let (term', _) = alpha 0 term
  putStrLn ("α > " ++ show_term term')
  let reduced : steps = beta_full term'
  print_steps steps
  case decode_nat reduced of
    Some num ->
      putStrLn ("Natural number detected: " ++ show num)
    None ->
      return ()


main :: IO ()
main = do
  file : _ <- getArgs
  run_file file
