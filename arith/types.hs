module Types
( Info(..)
, Term(..)
) where

data Info = Info String Int | Unknown deriving (Show)

data Term =
    TTrue Info
  | TFalse Info
  | TIf Info Term Term Term
  | TNum Info Int
  | TSucc Info Term
  | TPred Info Term
  | TIsZero Info Term

instance Show Term where
  show (TTrue _) = "true"
  show (TFalse _) = "false"
  show (TIf (Info _ line) t1 t2 t3) = (show line) ++ " if " ++ (show t1) ++ " then " ++ (show t2) ++ " else " ++ (show t3)
  show (TIf _ t1 t2 t3) = "if " ++ (show t1) ++ " then " ++ (show t2) ++ " else " ++ (show t3)
  show (TNum (Info _ line) n) = "<Line " ++ (show line) ++ "> "  ++ (show n)
  show (TNum _ n) = show n
  show (TSucc _ t) = show (1 + (read (show t) :: Int))
  show (TPred _ t) = show ((read (show t) :: Int) - 1)
  show (TIsZero _ t) = "iszero " ++ (show t)
