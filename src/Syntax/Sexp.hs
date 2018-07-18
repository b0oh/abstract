module Syntax.Sexp where

import Prelude hiding (read)
import qualified Data.Char as Char


operator_chars = "+-*/"


is_operator = (`elem` operator_chars)


is_white char = char `elem` " \n\t"


is_symbol char =
  Char.isDigit char
  || Char.isLetter char
  || char == '_'
  || is_operator char


is_symbol_delimiter char =
  is_white char
  || char `elem` "()"


skip_white = dropWhile is_white


data Sexp = Symbol String | List [Sexp] deriving Show

type ReaderState = (Sexp, String)


read_ :: String -> Sexp
read_ input =
  case read input of
    (syntax, _rest) ->
      syntax


read :: String -> ReaderState
read input@(char : rest)
  | is_white char =
    read rest
  | char == '(' =
    read_list rest []
  | is_symbol char =
    read_symbol rest [char]


read_symbol :: String -> String -> ReaderState
read_symbol input acc =
  case input of
    [] ->
      (Symbol (reverse acc), input)
    char : _ | is_symbol_delimiter char ->
      (Symbol (reverse acc), input)
    char : rest | is_symbol char ->
      read_symbol rest (char : acc)


read_list :: String -> [Sexp] -> ReaderState
read_list input acc =
  case input of
    char : rest | char == ')' ->
      (List (reverse acc), rest)
    _ ->
      case read input of
        (lexem, rest) ->
          read_list rest (lexem : acc)
