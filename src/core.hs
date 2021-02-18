import qualified Control.Monad.State as S
import qualified Control.Monad.Fail as F
import qualified Data.Map as M

type Variable = String
type Location = Integer

type Env = M.Map Variable Location

type Store = M.Map Location Val

data BaseVal = Int Integer
             | Double Double
             | Bool Bool

data Lambda = Lambda [Variable] Term

data Val = Sus
         | BaseVal BaseVal
         | Closure Env [Variable] Term
         | VariantVal { name :: String
                      , labels :: M.Map String Val
                      }

data Match = Binding Variable
           | Pattern { name :: String
                     , labels :: M.Map String Match
                     }

data Binding = Binding Variable Term

data LetDef = LetDef { bindings :: [Binding]
                     , body :: Term
                     }

data Term = Value BaseVal
          | Var Variable
          | Lambda { args ::[Variable]
                   , body :: Term
                   }
          | PrimOp { action :: Store -> [Val] -> Computation
                   , arity :: Integer
                   }
          | Variant { name :: String
                    , labels :: M.Map String Term
                    }
          | Call { fn :: Term
                 , args :: [Term]
                 }
          | Case { input :: Term
                 , [Match]
                 }
          | Let LetDef
          | LetRec LetDef

data DataDef = DataDef { name :: String
                       , labels :: [Strings]
                       }

data Program = Program [DataDef] Term

data Result a = Success a
            | Failure { errMsg :: String }

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap fn (Success a) = Success (fn a)
  fmap _ failure = failure

instance Applicative Result where
  pure :: a -> Result a
  pure a = Success a

  (<*>) :: Result (a -> b) -> Result a -> Result b
  failure@(Failure _) <*> _ = failure
  (Success fn) <*> a = fmap fn a

instance Monad Result where
  return :: a -> Result a
  return = pure

  (>>=) :: Result a -> (a -> Result b) -> Result b
  (Success a) >>= fn = fn a
  failure >>= _ = failure

instance F.MonadFail Result where
  fail :: String -> Result a
  fail msg = Failure msg

data ResultT m a = { runResultT :: m (Result a) }

instance (Monad m) => Functor (ResultT m) where
  fmap :: (a -> b) -> ResultT m a -> ResultT m b
  fmap fn m = ResultT $ do
    result <- runResultT m
    return $ do
      a <- result
      return (fn a)

instance (Monad m) => Applicative (ResultT m) where
  pure :: a -> ResultT m a
  pure = ResultT . return . return

  (<*>) :: ResultT m (a -> b) -> ResultT m a -> ResultT m b
  mfn <*> m = ResultT $ do
    fn <- runResultT mfn
    result <- runResultT m
    return (fn <*> result)

instance (Monad m) => Monad (ResultT m) where
  return :: a -> ResultT m a
  return = pure

  (>>=) :: ResultT m a -> (a -> ResultT m b) -> ResultT m b
  m >>= fn = ResultT $ do
    result <- runResultT m
    case result of
      failure@(Failure _) -> return failure
      (Success a) -> runResultT (fn a)

instance (Monad m) => F.MonadFail (ResultT m) where
  fail :: String -> ResultT m a
  fail msg = ResultT $ return (Failure msg)

data Computation = ResultT (S.State Store) Val
