module Syntax.Sexp where

import Prelude hiding (read)
import qualified Data.Char as Char


special_chars = "+*^/#,\\-_?!|>"


is_special = (`elem` special_chars)


is_white char = char `elem` " \n\t"


is_symbol_init char =
  is_special char
  || Char.isDigit char
  || Char.isLetter char


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
  | char == ';' =
    skip_comment_line rest
  | is_symbol_init char =
    read_symbol rest [char]


skip_comment_line :: String -> ReaderState
skip_comment_line input =
  let
    skipped = dropWhile (/= '\n') input
  in
    read skipped


read_symbol :: String -> String -> ReaderState
read_symbol input acc =
  case input of
    [] ->
      (Symbol (reverse acc), input)
    '\\' : escaped : rest ->
      read_symbol rest (escaped : '\\'  : acc)
    char : _ | is_symbol_delimiter char ->
      (Symbol (reverse acc), input)
    char : rest ->
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
