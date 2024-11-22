module Ch20 where

import Prelude

-- import Data.Identity (Identity)
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)
import Control.Monad.State.Trans (StateT, runStateT, get, put)
-- import Control.Monad.Writer.Trans (class MonadTell, WriterT, runWriterT, tell)


import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
-- import Data.Newtype (unwrap)

import Effect (Effect)
import Effect.Console as Console



newtype ReaderT r m a  = ReaderT (r -> m a)
{-
newtype StateT s m a   = StateT (s -> m (Tuple a s))
newtype ExceptT e m a = ExceptT(m (Either e a))
type Nested r s w a = ReaderT r (StateT s (WriterT w Identity)) a 

type Reader r = ReaderT r Identity
type State  s = StateT  s Identity 
type Writer w = WriterT w Identity


-- way to lift a monad into other

class MonadTrans t where
  lift :: ∀ m a. Monad m => t a -> t (m a)
-}

-- WriterT 
newtype WriterT w m a  = WriterT (m (Tuple a w)) 

runWriterT :: ∀ w m a. WriterT w m a -> m (Tuple a w)
runWriterT (WriterT mx) = mx

{-
  mx :: m (Tuple a w)
-}
instance functorWriterT :: Functor m => Functor (WriterT w m) where
  map :: ∀ a b. (a -> b) -> (WriterT w m a) -> (WriterT w m b)
  map f (WriterT mx) = WriterT $ mx <#> \(Tuple x w) -> Tuple (f x) w

{-
  mx :: m (Tuple a w)
  f :: m (Tuple (a->b) w)
-}
instance applyWriterT ::(Semigroup w, Monad m) => Apply (WriterT w m) where
  apply :: ∀ a b. WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
  apply (WriterT mf) (WriterT mx) = WriterT do
    Tuple f w  <- mf
    Tuple x w' <- mx
    pure $ Tuple (f x) (w <> w')

instance applicativeWriterT :: (Monoid w, Monad m) => Applicative (WriterT w m) where
  pure :: ∀ a. a -> WriterT w m a
  pure x = WriterT $ pure $ Tuple x mempty

instance bindWriterT :: (Semigroup w, Monad m) => Bind (WriterT w m) where
  bind :: ∀ a b. WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
  bind (WriterT mx) g = WriterT do
    Tuple x w  <- mx
    Tuple y w' <- runWriterT (g x)
    pure $ Tuple y (w <> w')

instance monadWriterT :: (Monoid w, Monad m) => Monad(WriterT w m)

type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a
type AppM = AppStack String String Int Unit

type StackResult = Tuple (Tuple (Either String Unit) String) Int

-- log :: ∀ m. MonadTell String m => String -> m Unit
-- log s = tell $ s <> "\n"

-- type AppEffects = 
--   { log :: String
--   , state :: Int
--   , result :: Maybe Unit
--   }

-- type AppResult = Tuple (Maybe String) AppEffects
-- results :: StackResult -> AppResult
-- results (Tuple (Tuple (Left err) l) s) 
--   = Tuple (Just err) { log : l, state: s, result: Nothing } 
-- results (Tuple (Tuple (Right _) l) s) 
--   = Tuple Nothing { log : l, state: s, result: Just unit } 
-- runApp :: Int -> AppM -> Effect AppResult
-- runApp s = map results
--   <<< flip runStateT s
--   <<< runWriterT
--   <<< runExceptT


-- app :: AppM 
-- app = do
--   log "Starting app"
--   n <- get
--   when (n == 0) $ void $throwError "WE CANNOT HAVE A 0 STATE!"
--   put $ n + 1
--   log "increment state"
--   pure unit

-- test :: Effect Unit
-- test = do
--   result1 <- runApp 0 app
--   Console.log $ show result1
--   result2 <- runApp 99 app
--   Console.log $ show result2

-- Remove later
test :: Effect Unit
test = do
  Console.log "CH20 placeholder"