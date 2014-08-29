{
module Parser (
  prettyRun,
) where

import Lexer
import Control.Monad
}

%name parse
%tokentype { Token }

%token
 lambda         { Slash _ }
 uscore         { Underscore _ }
 dot            { Dot _ }
 ';'            { Semicolon _ }
 '('            { OP _ }
 ')'            { CP _ }
 var            { Var _ _ }

%%

File : Stmts { \ctx -> let (stmts,ctx') = ($1 ctx) in (ParseResult stmts ctx') }

Stmts : Stmt ';' Stmts { \ctx -> let (stmts,ctx') = ($3 ctx) in (($1:stmts),ctx) }
     | {- empty -} { \ctx -> ([],ctx) }

Stmt : App { $1 }
     | lambda var dot Stmt { mkAbsNode $2 $4 }
     | lambda uscore dot Stmt { mkAbsNode $2 $4 }

App : Term { $1 }
    | App Term { mkAppNode $1 $2 }

Term : '(' Stmt ')' { $2 }
      | var { mkVarNode $1  }

{

type ParseNode = Context -> Term
data ParseResult = ParseResult [ParseNode] Context

instance Show ParseResult where
  show (ParseResult nodes cxt) = "ParseResult<" ++ (show (map (\f -> f cxt) nodes)) ++ ">"

-- Parsing Helpers

mkVarNode :: Token -> ParseNode
mkVarNode (Var info s) = \ctx -> 
  case (indexOfName ctx s) of
    Just idx -> TVar info idx (contextLength ctx)
    Nothing  -> TFail info ("variable name lookup failure: " ++ s)
mkVarNode t = \ctx -> TFail (getTokenInfo t) "incorrect token to make variable node"

mkAbsNode :: Token -> ParseNode -> ParseNode
mkAbsNode (Var info s) n =
  \ctx -> let ctx' = addName ctx s in
    TAbs info s (n ctx')
mkAbsNode (Underscore info) n =
  \ctx -> let ctx' = addName ctx "_" in
    TAbs info "_" (n ctx')
mkAbsNode t _ = \ctx -> TFail (getTokenInfo t) "incorrect token to make abstraction node"

mkAppNode :: ParseNode -> ParseNode -> ParseNode
mkAppNode n1 n2 = \ctx ->
  let t1 = n1 ctx
      t2 = n2 ctx in
    TApp t1 t2

-- TAbs has the String used in the actual program text as well as the term under the abstraction
-- TVar has the index of the variable as well as the length of the context it was defined in
data Term =
    TAbs Info String Term
  | TApp Term Term
  | TVar Info Int Int
  | TFail Info String
  deriving (Show)

-- Wrapper

data Result = Result Term Context

instance Show Result where
  show (Result (TAbs i s val) ctx) =
    let (ctx', s') = pickFreshName ctx s 
        term = show (Result val ctx') in
        "\\" ++ s' ++ ". " ++ term
  show (Result (TApp val1@(TVar _ _ _) val2@(TVar _ _ _)) ctx) = (show (Result val1 ctx)) ++ " " ++ (show (Result val2 ctx))
  show (Result (TApp val1@(TVar _ _ _) val2) ctx) = (show (Result val1 ctx)) ++ " (" ++ (show (Result val2 ctx)) ++ ")"
  show (Result (TApp val1 val2@(TVar _ _ _)) ctx) = "((" ++ (show (Result val1 ctx)) ++ ") " ++ (show (Result val2 ctx)) ++ ")"
  show (Result (TApp val1 val2) ctx) = "(" ++ (show (Result val1 ctx)) ++ ") (" ++ (show (Result val2 ctx)) ++ ")" 
  show (Result (TVar i k len) ctx) =
    case nameOfIndex ctx k of
      Nothing -> error "Unknown variable"
      Just s -> s
  show (Result (TFail i msg) ctx) = error ("Result show failure (" ++ (show i) ++ "): " ++ msg)

-- Context Helpers

type Context = [(String, Binding)]
data Binding = NameBinding

emptyContext :: Context
emptyContext = []

contextLength :: Context -> Int
contextLength = length

nthBinding :: Context -> Int -> Maybe (String, Binding)
nthBinding ctx n =
  if n < (contextLength ctx)
    then (Just (ctx !! n))
    else Nothing

addName :: Context -> String -> Context
addName ctx s = addBinding ctx s NameBinding

addBinding :: Context -> String -> Binding -> Context
addBinding ctx s binding = (s, binding):ctx

isNameBound :: Context -> String -> Bool
isNameBound [] _ = False
isNameBound ((x,_):xs) s
  | x == s    = True
  | otherwise = isNameBound xs s

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx s =
  if isNameBound ctx s
  then pickFreshName ctx (s ++ "'")
  else ((addName ctx s), s)

nameOfIndex :: Context -> Int -> Maybe String
nameOfIndex ctx n = (nthBinding ctx n) >>= \x -> Just (fst x)

indexOfName :: Context -> String -> Maybe Int
indexOfName [] _ = Nothing
indexOfName ((x,_):xs) s
  | x == s    = Just 0
  | otherwise = case indexOfName xs s of
    Just n  -> Just (1 + n)
    Nothing -> Nothing

isValue :: Context -> Term -> Bool
isValue _ (TAbs _ _ _) = True
isValue _ _ = False

-- Substitution

shifty :: (Info -> Int -> Int -> Int -> Term) -> Int -> Term -> Term
shifty f k v =
  let walk c (TVar i x n)  = f i c x n
      walk c (TAbs i x t') = (TAbs i x (walk (c+1) t'))
      walk c (TApp t1 t2)  = (TApp (walk c t1) (walk c t2))
  in (walk k v)

termShift' :: Int -> Int -> Term -> Term
termShift' d c t = shifty (\i n x k ->
  if x >= n
  then (TVar i (x+d) (k+d))
  else (TVar i x (k+d))) c t

termShift :: Int -> Term -> Term
termShift d t = termShift' d 0 t

termSubst' :: Int -> Term -> Term -> Term
termSubst' j v t = shifty (\i n x k ->
  if x==j+n
  then termShift n v
  else (TVar i x k)) 0 t

termSubst :: Term -> Term -> Term
termSubst val intoTerm = termShift (-1) (termSubst' 0 (termShift 1 val) intoTerm)

-- Evaluation

eval1 :: Context -> Term -> Maybe Term
eval1 ctx (TApp (TAbs _ x t) v)
  | isValue ctx v = Just (termSubst v t)
eval1 ctx (TApp v t)
  | isValue ctx v = case eval1 ctx t of
    Nothing -> Nothing
    Just t' -> Just (TApp v t')
eval1 ctx (TApp t t2) =
  case eval1 ctx t of
    Nothing -> Nothing
    Just t' -> Just (TApp t' t2)
eval1 _ _ = Nothing

eval :: Context -> Term -> Term
eval ctx v = case eval1 ctx v of
  Nothing -> v
  Just v' -> eval ctx v'

-- Run the parser

runP :: String -> [Term]
runP s = let (ParseResult vs ctx) = (parse . alexScanTokens) s emptyContext in
  map (\f -> eval ctx (f ctx)) vs

prettyRun :: String -> [Result]
prettyRun s = map (\v -> Result v emptyContext) $ runP s 

-- Errors

happyError :: [Token] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\nTokens: " ++ (show tks) ++ "\n")
    where
    lcn =       case tks of
          [] -> "end of file"
          t:_ -> "line " ++ show l ++ ", column " ++ show c
            where
             (Info l c) = getTokenInfo t
}
