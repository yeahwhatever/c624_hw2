import Control.Monad.Error hiding (throwError)

-- Boring defines
data Term = TmVar Int 
    | TmAbs Term 
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
isval t = case t of 
                  TmAbs _ -> True
                  _ -> False

-- Black magic from the book
termShift :: Int -> Term -> Term
termShift d t = 
	let walk c t = case t of
			TmVar x -> if x>=c then TmVar (x+d) 
					else TmVar x 
			TmAbs t1 -> TmAbs (walk (c+1) t1)
			TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
	in walk 0 t

termSubst :: Int -> Term -> Term -> Term
termSubst j s t =
	let walk c t = case t of
			TmVar x -> if x==j+c then termShift c s 
					else TmVar x 
			TmAbs t1 -> TmAbs (walk (c+1) t1)
			TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
	in walk 0 t

termSubstTop :: Term -> Term -> Term
termSubstTop s t =
	termShift (-1) (termSubst 0 (termShift 1 s) t)

-- Eval!
eval :: Term -> ThrowsError Term
eval (TmAbs x) = return $ TmAbs x
eval (TmApp t1 t2) = do t3 <- eval t1
                        v1 <- eval t2
                        case t3 of
                          TmAbs e -> eval $ termSubstTop v1 t3
                          _ -> throwError "Cannot apply Term to non-TmAbs"

-- Being able to print stuff is cool
termString :: Term -> String
termString (TmAbs t) = "(lambda "++(termString t)++")"
termString (TmApp t1 t2) = "("++(termString t1)++" "++(termString t2)++")"
termString (TmVar i) = show i

-- Lets define some stuff to test!
false = (TmAbs (TmAbs(TmVar 0)))
zero = (TmAbs (TmAbs(TmVar 0)))
one = (TmAbs (TmAbs( TmApp (TmVar 1) (TmVar 0))))

main = let v = eval (TmApp (TmApp(false) (one))(zero))
          in case v of 
            Left err -> putStrLn (show err)
            Right t -> putStrLn (termString t)

