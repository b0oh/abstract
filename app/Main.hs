module Main where

import System.Environment

import Optional
import Term
import Show
import Syntax.Sexp
import Syntax.Scheme


run_file :: String -> IO ()
run_file file_name = do
  contents <- readFile file_name
  let sexp = read_ contents
  let term = extract sexp
  putStrLn ("λ > " ++ show_term term)
  let normal_form = full_beta term
  putStrLn ("λ > " ++ show_term normal_form)
  case decode_nat normal_form of
    Some num ->
      putStrLn ("Natural number detected: " ++ show num)
    None ->
      return ()


main :: IO ()
main = do
  file : _ <- getArgs
  run_file file
