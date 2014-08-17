> module ParseMonad where

We first define some data types to represent the result of parsing, in this case
we can either have a success or a failure. We could have used `Either` but, the
Ok and Failed are more descriptive and I copied this from the Happy docs.

> data ParseResult a = OkP a | FailedP String
> type LineNumber = Int
> newtype P a = P (String -> LineNumber -> ParseResult a)

> getLineNo :: P LineNumber
> getLineNo = P $ \_ l -> OkP l

> runP :: P a -> String -> Int -> ParseResult a
> runP (P f) = f

> instance Monad P where
>   return a = P $ \_ _ -> OkP a
>   m >>= k = P $ \s l -> case runP m s l of
>       OkP a -> runP (k a) s l
>       FailedP err -> FailedP err
>   fail s = P $ \_ _ -> FailedP s
