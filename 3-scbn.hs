import Control.Monad.Error hiding (throwError)

data Term = TmLet String Term Term
    | TmIf Term Term Term
    | TmVar String
    | TmTrue
    | TmFalse
    | TmNat Int
    | TmApply Term Term
    | TmLambda String Term Env

-- Static Call by Name
data Env = Env [(String, Term, Env)]

addEnv :: String -> Term -> Env -> Env -> Env
addEnv k v t (Env env) = Env $ [(k, v, t)] ++ [(a, b, e) | (a, b, e) <- env, a /= k]

getEnv :: String -> Env -> Maybe (Term, Env)
getEnv k (Env ((k',v,e):xs)) = if k == k' then Just (v, e) else getEnv k (Env xs)
getEnv k (Env []) = Nothing

eval :: Term -> Env -> Maybe Term
eval (TmLet k v t) env      = eval t (addEnv k v env env)
eval (TmIf c t f) env       = do c' <- eval c env
                                 case c' of
                                      TmTrue -> eval t env
                                      TmFalse -> eval f env
                                      _ -> Nothing

eval (TmApply f v) env      = do 
                                f' <- eval f env
                                case f' of
                                     TmLambda n body env' -> eval body (addEnv n v env env')
                                     _ -> Nothing

eval (TmNat n) _            = return $ TmNat n
eval (TmVar id) env         = do (v, e) <- getEnv id env
                                 eval v e
eval (TmLambda s t _) env   = return $ TmLambda s t env
eval TmTrue _               = return TmTrue
eval TmFalse _              = return TmFalse
