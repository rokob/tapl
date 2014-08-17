module Types
( Info(..)
, Term(..)
) where

data Info = Info String Int Int | Unknown deriving (Show)

data Term =
    TTrue Info
  | TFalse Info
  | TIf Info Term Term Term
  | TZero Info
  | TSucc Info Term
  | TPred Info Term
  | TIsZero Info Term

instance Show Term where
  show (TTrue _) = "true"
  show (TFalse _) = "false"
  show (TIf _ t1 t2 t3) = "if " ++ (show t1) ++ " then " ++ (show t2) ++ " else " ++ (show t3)
  show (TZero _) = "0"
  show (TSucc _ (TZero _)) = "1"
  show (TSucc _ t) = show (1 + (read (show t) :: Int))
  show (TPred _ (TZero _)) = "0"
  show (TPred _ (TSucc _ (TZero _))) = "0"
  show (TPred _ t) = show ((read (show t) :: Int) - 1)
  show (TIsZero _ t) = "iszero " ++ (show t)
