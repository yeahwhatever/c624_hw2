import Control.Monad.Error hiding (throwError)

data Term = TmLet String Term Term
    | TmIf Term Term Term
    | TmVar String
    | TmTrue
    | TmFalse
    | TmNat Int
    | TmApply Term Term
    | TmLambda String Term 

-- Static Call by Value   
type Env = [(String, Term)]

addEnv :: String -> Term -> Env -> Env
addEnv k v env = [(k, v)] ++ [(a, b) | (a, b) <- env, a /= k]

getEnv :: String -> Env -> Maybe Term
getEnv k env = lookup k env

eval :: Term -> Env -> Maybe Term
eval (TmLet k v t) env      = eval t (addEnv k v env)
eval (TmIf c t f) env       = do c' <- eval c env
                                 case c' of
                                      TmTrue -> eval t env
                                      TmFalse -> eval f env
                                      _ -> Nothing

eval (TmApply f v) env      = do 
                                f' <- eval f env
                                case f' of
                                     TmLambda n body -> eval body (addEnv n v env)
                                     _ -> Nothing

eval (TmNat n) _            = return $ TmNat n
eval (TmVar id) env         = do v <- getEnv id env
                                 eval v env
eval (TmLambda s t) env     = return $ TmLambda s t 
eval TmTrue _               = return TmTrue
eval TmFalse _              = return TmFalse
