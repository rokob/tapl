module Tokens where

data Token
  = TokenNumber Int TokenId
  | TokenKW TokenId
  | TokenEOF

tokenToId :: Token -> TokenId
tokenToId (TokenNumber _ i) = i
tokenToId (TokenKW i) = i
tokenToId TokenEOF = error "tokenToId TokenEOF"

instance Eq Token where
      i == i' = tokenToId i == tokenToId i'

instance Ord Token where
      i <= i' = tokenToId i <= tokenToId i'

data TokenId
  = TokenIf
  | TokenThen
  | TokenElse
  | TokenNum
  | TokenIsZero
  | TokenPred
  | TokenSucc
  | TokenTrue
  | TokenFalse
  | TokenOP
  | TokenCP
  | TokenSemi
  deriving (Eq,Ord,Show)
