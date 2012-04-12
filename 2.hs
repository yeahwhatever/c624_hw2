import Control.Monad.Error hiding (throwError)

data Term = TmVar Int
    | TmAbs String Term 
    | TmApp Term Term deriving Show

data TaplError = ParserError String
               | EvalError String
               | TypeMismatch String
               | UndefinedVariable String
               | Default String deriving Show

type ThrowsError = Either TaplError

throwError :: String -> ThrowsError a
throwError x = Left (EvalError x)

isval :: Term -> Bool
isval (t) -> case t of 
                  TmAbs(_,_,_) -> True
                  _ -> False

