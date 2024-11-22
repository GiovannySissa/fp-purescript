module Ch20 where

import Prelude

import Data.Identity (Identity)
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)
import Control.Monad.State.Trans (StateT, runStateT, get, put)
import Control.Monad.Writer.Trans (class MonadTell, WriterT, runWriterT, tell)


import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)

import Effect (Effect)
import Effect.Console as Console

{-
newtype ReaderT r m a  = ReaderT (r -> m a)
newtype StateT s m a   = StateT (s -> m (Tuple a s))
newtype WriterT w m a  = WriterT (m (Tuple a w)) 
newtype ExceptT e m a = ExceptT(m (Either e a))
type Nested r s w a = ReaderT r (StateT s (WriterT w Identity)) a 

type Reader r = ReaderT r Identity
type State  s = StateT  s Identity 
type Writer w = WriterT w Identity


-- way to lift a monad into other

class MonadTrans t where
  lift :: ∀ m a. Monad m => t a -> t (m a)
-}

type AppStack e w s a = ExceptT e (WriterT w (StateT s Identity)) a
type AppM = AppStack String String Int Unit

type StackResult = Tuple (Tuple (Either String Unit) String) Int

log :: ∀ m. MonadTell String m => String -> m Unit
log s = tell $ s <> "\n"

type AppEffects = 
  { log :: String
  , state :: Int
  , result :: Maybe Unit
  }

type AppResult = Tuple (Maybe String) AppEffects
results :: StackResult -> AppResult
results (Tuple (Tuple (Left err) l) s) 
  = Tuple (Just err) { log : l, state: s, result: Nothing } 
results (Tuple (Tuple (Right _) l) s) 
  = Tuple Nothing { log : l, state: s, result: Just unit } 
runApp :: Int -> AppM -> AppResult
runApp s = 
  results
  <<< unwrap 
  <<< flip runStateT s
  <<< runWriterT
  <<< runExceptT


app :: AppM 
app = do
  log "Starting app"
  n <- get
  when (n == 0) $ void $throwError "WE CANNOT HAVE A 0 STATE!"
  put $ n + 1
  log "increment state"
  pure unit

test :: Effect Unit
test = do
  Console.log $ show $ runApp 0 app
  Console.log $ show $ runApp 99 app
