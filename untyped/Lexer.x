{
module Lexer (alexScanTokens, Info(..), Token(..), getTokenInfo) where
}

%wrapper "posn"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters

tokens :-

  $white+                               ;
  "--".*                                ;
  \;                                    { \(AlexPn a l c) _ -> Semicolon (Info l c) }
  \.                                    { \(AlexPn a l c) _ -> Dot (Info l c) }
  _                                     { \(AlexPn a l c) _ -> Underscore (Info l c) }
  \\                                    { \(AlexPn a l c) _ -> Slash (Info l c) }
  \(                                    { \(AlexPn a l c) _ -> OP (Info l c) }
  \)                                    { \(AlexPn a l c) _ -> CP (Info l c) }
  $alpha [$alpha $digit \_ \']*         { \(AlexPn a l c) s -> Var (Info l c) s }

{
data Info = Info Line Column deriving (Eq)
type Line = Int
type Column = Int

instance Show Info where
  show (Info l c) = "Line: " ++ (show l) ++ ", Column: " ++ (show c)

data Token = 
    Dot Info
  | Underscore Info
  | Semicolon Info
  | Slash Info
  | OP Info
  | CP Info
  | Var Info String
  deriving (Eq, Show)

getTokenInfo :: Token -> Info
getTokenInfo (Dot i) = i
getTokenInfo (Underscore i) = i
getTokenInfo (Semicolon i) = i
getTokenInfo (Slash i) = i
getTokenInfo (OP i) = i
getTokenInfo (CP i) = i
getTokenInfo (Var i _) = i
}
