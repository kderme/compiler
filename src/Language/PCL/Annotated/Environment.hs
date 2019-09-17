{-# LANGUAGE DeriveFunctor, DeriveAnyClass #-}

module Language.PCL.Annotated.Environment where

import           Prelude

import qualified Data.Map.Strict as M
import qualified Control.Monad.State.Strict as ST

import Language.PCL.Annotated.Lexer hiding (tok)
import Language.PCL.Annotated.Parser
import Language.PCL.Token
import Language.PCL.Annotated.Syntax
import Text.Parsec hiding (State)
import Text.Parsec.Pos
import Text.Parsec.Tok


type Tp = Type ()
type Hd = Header ()
type Id = Name SourcePos

data Fun = Fun {
    srcPos :: SourcePos
  , fargs  :: [(Bool, String, Tp, SourcePos)]
  , fret   :: Maybe Tp
  } deriving (Show, Eq)

data Env = VEnv Tp SourcePos
         | LEnv SourcePos
         | FunEnv Fun
         | ForEnv Fun
         deriving (Show, Eq)

data Environment = Environment {
    env  :: M.Map String Env
  , incr :: Int
  } deriving (Show, Eq)

getPos :: Env -> SourcePos
getPos (VEnv _ pos) = pos
getPos (LEnv pos) = pos
getPos (FunEnv fun) = srcPos fun
getPos (ForEnv fun) = srcPos fun


emptyEnv :: Environment
emptyEnv = Environment {
    env  = M.empty
  , incr = 0
  }

initEnv :: SourceName -> Environment
initEnv sn = emptyEnv {
    env = M.fromList [ ("sqrt", mkfunReal)
                     , ("readInteger", readInteger)
                     , ("writeString", writeString)
                     , ("writeInteger", writeInteger)
                     ]
    }
    where
        mkfunReal = realToReal sn

        readInteger = FunEnv $ Fun {
            srcPos = initialPos sn
          , fargs = []
          , fret = Just $ TInt ()
        }

        writeString = FunEnv $ Fun {
              srcPos = initialPos sn
            , fargs = [(True, "s", TArray () Nothing (TChar ()), initialPos sn)]
            , fret = Nothing
            }

        writeInteger = FunEnv $ Fun {
              srcPos = initialPos sn
            , fargs = [(False, "n", TInt (), initialPos sn)]
            , fret = Nothing
            }

realToReal :: SourceName -> Env
realToReal sn = FunEnv $ Fun {
      srcPos = initialPos sn
    , fargs = [(False, "r", TReal (), initialPos sn)]
    , fret = Just $ TReal ()
    }

newtype StateMonad a = StateMonad (Environment -> Either String (a, Environment))
    deriving (Functor, Applicative)

-- instance Applicative StateMonad where
--     pure a = StateMonad $ \s -> Right (a, s)
--     StateMonad fp (<*>) StateMonad p = 


instance Monad StateMonad where
    (StateMonad p) >>= k =
        StateMonad $ \s0 ->
            case p s0 of
                Right (val, s1) ->
                    let (StateMonad q) = k val
                     in q s1
                Left e -> Left e
    return a = StateMonad $ \s -> Right (a, s)

runState :: StateMonad a -> Environment -> Either String (a, Environment)
runState (StateMonad p) = p

get :: StateMonad Environment
get = StateMonad $ Right . (\s -> (s,s))

put :: Environment -> StateMonad ()
put s = StateMonad $ Right . (\_ -> ((), s))

modify' :: (Environment -> Environment) -> StateMonad ()
modify' f = do
    s <- get
    put $! f s

failEnv :: String -> StateMonad ()
failEnv str = StateMonad $ \_ -> Left str

liftEither :: Either String a -> StateMonad ()
liftEither (Left str) = failEnv str
liftEither (Right _) = return ()
