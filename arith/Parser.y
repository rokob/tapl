{
module Parser (
  parse,
  runP,
  ParseResult(..)
) where

import ParseMonad
import Lexer
import Tokens
import Types
}

%name parse
%tokentype { Token }
%monad { P }
%lexer { lexer } { TokenEOF }

%token
  if            { TokenKW TokenIf }
  then          { TokenKW TokenThen }
  else          { TokenKW TokenElse }
  int           { TokenNumber $$ TokenNum }
  iszero        { TokenKW TokenIsZero }
  pred          { TokenKW TokenPred }
  succ          { TokenKW TokenSucc }
  true          { TokenKW TokenTrue }
  false         { TokenKW TokenFalse }
  '('           { TokenKW TokenOP }
  ')'           { TokenKW TokenCP }
  ';'           { TokenKW TokenSemi }

%%

ExpList : Exp ';' ExpList { $1 : $3 }
        | {- empty -} { [] }

Exp    : if Exp then Exp else Exp  {% getLineNo >>= \l -> return (TIf (Info "" l) $2 $4 $6) }
       | pred Exp { TPred Unknown $2 }
       | succ Exp { TSucc Unknown $2 }
       | iszero Exp { TIsZero Unknown $2 }
       | Value { $1 }

Value  : true  { TTrue Unknown }
       | false { TFalse Unknown }
       | int   {% getLineNo >>= \l -> return (TNum (Info "" l) $1) }
       | '(' Exp ')' { $2 }

{


happyError :: P a
happyError = getLineNo >>= \l -> fail (show l ++ ": Parse error\n")

}
