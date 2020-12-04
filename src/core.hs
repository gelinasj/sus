import qualified Data.Map as M


type Variable = String

data BaseVal := Int Integer
              | Double Double
              | Bool Bool

data Lambda := Lambda [Variable] Term

data Val := Thunk
          | BaseVal BaseVal
          | Closure Env [Variable] Term
          | VariantVal { name :: String
                       , labels :: Map String Val
                       }

data Match := Binding Variable
            | Pattern { name :: String
                      , labels :: Map String Match
                      }

data Binding := Binding Variable Term

data Term := Value BaseVal
           | Var Variable
           | Lambda { args ::[Variable]
                    , body :: Term
                    }
           | PrimOp { action :: [Val] -> Result Val
                    , arity :: Integer
                    }
           | Variant { name :: String
                     , labels :: Map String Term
                     }
           | Call { fn :: Term
                  , args :: [Term]
                  }
           | Case { input :: Term
                  , [Match]
                  }
           | Let [Binding]
           | LetRec [Binding]
