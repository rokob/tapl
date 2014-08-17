module Lexer where

import ParseMonad
import Tokens
import Data.Char

lexer :: (Token -> P a) -> P a
lexer cont = P lexer'
  where lexer' "" = returnToken cont TokenEOF ""
        lexer' ('/':'/':r) = lexer' (dropWhile (/= '\n') r)
        lexer' ('/':'*':r) = \line -> lexNestedComment line lexer' r line
        lexer' (c:rest) = nextLex cont c rest

returnToken :: (t -> P a) -> t -> String -> LineNumber -> ParseResult a
returnToken cont tok = runP (cont tok)

nextLex :: (Token -> P a) -> Char -> String -> LineNumber -> ParseResult a
nextLex cont c = case c of
      '\n'    -> \rest line -> returnToken lexer cont rest (line+1)
      ';'     -> returnToken cont (TokenKW TokenSemi)
      '('     -> returnToken cont (TokenKW TokenOP)
      ')'     -> returnToken cont (TokenKW TokenCP)

      _
        | isSpace c -> runP (lexer cont)
        | isDigit c -> lexNum cont c
        |  c >= 'a' && c <= 'z'
          || c >= 'A' && c <= 'Z' -> lexVar cont c

      _       -> lexError ("lexical error before `" ++ c : "'")

lexNum :: (Token -> P a) -> Char -> String -> LineNumber -> ParseResult a
lexNum cont c rest =
       readNum rest (\ num rest' ->
                        returnToken cont (TokenNumber (stringToInt (c:num)) TokenNum) rest')
 where stringToInt = foldl (\n c' -> digitToInt c' + 10*n) 0

readNum :: String -> (String -> String -> a) -> a
readNum (c:r) fn | isDigit c = readNum r (fn . (:) c)
readNum r     fn = fn [] r

lexVar :: (Token -> P a) -> Char -> String -> LineNumber -> ParseResult a
lexVar cont c rest =
  case (c:rest) of
    'i':'f':rest' -> returnToken cont (TokenKW TokenIf) rest' 
    't':'h':'e':'n':rest' -> returnToken cont (TokenKW TokenThen) rest' 
    'e':'l':'s':'e':rest' -> returnToken cont (TokenKW TokenElse) rest' 
    'i':'s':'z':'e':'r':'o':rest' -> returnToken cont (TokenKW TokenIsZero) rest' 
    'p':'r':'e':'d':rest' -> returnToken cont (TokenKW TokenPred) rest' 
    's':'u':'c':'c':rest' -> returnToken cont (TokenKW TokenSucc) rest' 
    't':'r':'u':'e':rest' -> returnToken cont (TokenKW TokenTrue) rest' 
    'f':'a':'l':'s':'e':rest' -> returnToken cont (TokenKW TokenFalse) rest' 
    _ -> lexError ("unrecognised term: " ++ takeWhile (not.isSpace) (c:rest)) (c:rest)

lexNestedComment :: LineNumber -> (String -> LineNumber -> ParseResult a) -> String -> LineNumber -> ParseResult a
lexNestedComment l cont r =
  case r of
      '*':'/':r' -> cont r'
      '/':'*':r' -> \line -> lexNestedComment line
                      (\r'' -> lexNestedComment l cont r'') r' line
      '\n':r'    -> \line -> lexNestedComment l cont r' (line+1)
      _:r'       -> lexNestedComment l cont r'
      ""         -> \_ -> lexError "unterminated comment" r l

lexError :: String -> String -> LineNumber -> ParseResult a
lexError err = runP (getLineNo >>= \l -> fail (show l ++ ": " ++ err ++ "\n"))
