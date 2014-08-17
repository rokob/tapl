import Types
import Parser
import System.IO
import System.Environment
import Data.List

isnumeric :: Term -> Bool
isnumeric (TNum _ _) = True
isnumeric (TSucc _ t) = isnumeric t
isnumeric _ = False

isval :: Term -> Bool
isval (TTrue _) = True
isval (TFalse _) = True
isval t
  | isnumeric t = True
  | otherwise = False

evalT1 :: Term -> Maybe Term
evalT1 (TIf _ (TTrue _) t2 t3) = Just t2
evalT1 (TIf _ (TFalse _) t2 t3) = Just t3
evalT1 (TIf info t1 t2 t3) =
  case evalT1 t1 of
    Just t1' -> Just (TIf info t1' t2 t3)
    Nothing -> Nothing
evalT1 (TSucc info t) =
  case evalT1 t of
    Just t' -> Just (TSucc info t')
    Nothing -> Nothing
evalT1 (TPred _ (TNum _ 0)) = Just (TNum Unknown 0)
evalT1 (TPred _ (TSucc _ n))
  | isnumeric n = Just n
evalT1 (TPred info t) =
  case evalT1 t of
    Just t' -> Just (TPred info t')
    Nothing -> Nothing
evalT1 (TIsZero _ (TNum _ 0)) = Just (TTrue Unknown)
evalT1 (TIsZero _ (TSucc _ n))
  | isnumeric n = Just (TFalse Unknown)
evalT1 (TIsZero info t) =
  case evalT1 t of
    Just t' -> Just (TIsZero info t')
    Nothing -> Nothing
evalT1 _ = Nothing

eval :: Term -> Term
eval t = case evalT1 t of
  Just t' -> eval t'
  Nothing -> t

main = do
  args <- getArgs
  let filename = head args
  handle <- openFile filename ReadMode
  contents <- hGetContents handle  
  let result = runP parse contents 1
  case result of
    OkP result' -> mapM_ putStrLn (map (show . eval) result')
    FailedP err -> putStrLn err
