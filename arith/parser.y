{
module Parser where

import Types
import Data.Char
}

%name parseInner
%tokentype { Token }
%error { parseError }

%token
  if            { TokenIf }
  then          { TokenThen }
  else          { TokenElse }
  '0'           { TokenZero }
  '1'           { TokenOne }
  iszero        { TokenIsZero }
  pred          { TokenPred }
  succ          { TokenSucc }
  true          { TokenTrue }
  false         { TokenFalse }
  '('           { TokenOP }
  ')'           { TokenCP }
  ';'           { TokenSemi }

%%

Exp    : if Exp1 then Exp1 else Exp1 ';'  { TIf Unknown $2 $4 $6 }
       | Value ';' { $1 }
       | iszero Exp1 ';' { TIsZero Unknown $2 }
       | Exp1 ';' { $1 }

Exp1   : pred Exp1 { TPred Unknown $2 }
       | succ Exp1 { TSucc Unknown $2 }
       | Value { $1 }

Value  : true  { TTrue Unknown }
       | false { TFalse Unknown }
       | '0'   { TZero Unknown  }
       | '1'   { TSucc Unknown (TZero Unknown)  }
       | '(' Exp1 ')' { $2 }

{

data Token
  = TokenIf
  | TokenThen
  | TokenElse
  | TokenZero
  | TokenOne
  | TokenIsZero
  | TokenPred
  | TokenSucc
  | TokenTrue
  | TokenFalse
  | TokenOP
  | TokenCP
  | TokenSemi
  deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isComment c cs = lexComment (tail cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
lexer ('0':cs) = TokenZero : lexer cs
lexer ('1':cs) = TokenOne : lexer cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer (';':cs) = TokenSemi : lexer cs

lexVar cs =
   case span isAlpha cs of
      ("if",rest) -> TokenIf : lexer rest
      ("then",rest)  -> TokenThen : lexer rest
      ("else",rest)  -> TokenElse : lexer rest
      ("iszero",rest)  -> TokenIsZero : lexer rest
      ("pred",rest)  -> TokenPred : lexer rest
      ("succ",rest)  -> TokenSucc : lexer rest
      ("true",rest)  -> TokenTrue : lexer rest
      ("false",rest)  -> TokenFalse : lexer rest

isComment '/' ('*':xs) = True
isComment _ _ = False

lexComment "*/" = lexer []
lexComment ('*':'/':xs) = lexer xs
lexComment (x:xs) = lexComment xs

parseError :: [Token] -> a
parseError _ = error "Parse error"

parse :: [Token] -> Maybe Term
parse [] = Nothing
parse ts = Just (parseInner ts)

}
