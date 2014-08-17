import Types
import Parser
import System.IO
import System.Environment
import Data.List

isnumeric :: Term -> Bool
isnumeric (TZero _) = True
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
evalT1 (TPred _ (TZero _)) = Just (TZero Unknown)
evalT1 (TPred _ (TSucc _ n))
  | isnumeric n = Just n
evalT1 (TPred info t) =
  case evalT1 t of
    Just t' -> Just (TPred info t')
    Nothing -> Nothing
evalT1 (TIsZero _ (TZero _)) = Just (TTrue Unknown)
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

cleanAST :: [Maybe Term] -> [Term]
cleanAST [] = []
cleanAST (Nothing:ts) = cleanAST ts
cleanAST ((Just t):ts) = t : cleanAST ts

main = do
  args <- getArgs
  let filename = head args
  handle <- openFile filename ReadMode
  contents <- hGetContents handle  
  let expressions = filter (\x -> x /= "") (lines contents)
      ast = map (parse . lexer) expressions
      cleaned = cleanAST ast
      result = map eval cleaned
  mapM_ putStrLn (map show result)
--  print $ eval (TTrue Unknown)
--  print $ eval (TIf Unknown (TFalse Unknown) (TTrue Unknown) (TFalse Unknown))
--  print $ eval (TZero Unknown)
--  print $ eval (TSucc Unknown (TPred Unknown (TZero Unknown)))
--  print $ eval (TIsZero Unknown (TPred Unknown (TSucc Unknown (TSucc Unknown (TZero Unknown)))))
