module Ch21 where

import Prelude

import Data.Tuple (Tuple (..))
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Error.Class (class MonadThrow, class MonadError, throwError, catchError)

import Effect (Effect)
import Effect.Console (log)

-- newtype ReaderT r m a = ReaderT (r -> m a)
-- newtype WriterT w m a = WriterT (m (Tuple a w))
newtype StateT s m a = StateT (s -> m (Tuple a s))

runStateT :: ∀ s m a. StateT s m a -> (s -> m (Tuple a s))
runStateT (StateT f) = f


instance functorStateT :: Functor m => Functor (StateT s m) where
  map f (StateT mf) = StateT \s -> 
    mf s <#> \(Tuple x s') ->  Tuple (f x) s' 

{-
  mf :: s -> m (Tuple (a->b) s)
  mg :: s -> m (Tuple a s)
-}
instance applyStateT :: Monad m => Apply (StateT s m) where
  apply :: ∀ a b. StateT s m (a -> b) -> StateT s m a -> StateT s m b
  apply (StateT mf) (StateT mg) = StateT \ s -> do
    Tuple f s'  <- mf s
    Tuple x s'' <-  mg s'
    pure $Tuple (f x) s''

  -- apply (StateT mf) (StateT mg) = StateT \ s ->
  --    mf s 
  --     >>= \(Tuple f s') ->  mg s' 
  --       <#> \(Tuple x s'') -> Tuple (f x) s''

instance applicativeStateT :: Monad m => Applicative (StateT s m) where
  pure x = StateT \s -> pure $ Tuple x s

instance bindStateT :: Monad m => Bind (StateT s m) where
  bind (StateT fmg) mf = StateT \s -> do
    Tuple x s' <- fmg s
    runStateT (mf x) s'

instance monadStateT :: Monad m => Monad (StateT s m)

instance monadStateStateT :: Monad m => MonadState s (StateT s m) where
  state :: ∀ a. (s -> Tuple a s) -> StateT s m a
  state f = StateT $ pure <<< f

instance monadAskMonadState :: MonadAsk r m => MonadAsk r (StateT s m) where 
  ask :: StateT s m r
  -- ask = liftStateT ask -- using custom implementation
  ask = lift ask

instance monadTellStateT :: MonadTell w m => MonadTell w (StateT s m) where
  tell:: w -> StateT s m Unit
  -- tell w = liftStateT $ tell w
  -- tell = liftStateT <<< tell
  tell = lift <<< tell

 
-- liftStateT :: ∀ s m a. Functor m => m a -> StateT s m a
-- liftStateT mx = StateT \s -> mx <#> \x -> Tuple x s
instance monadTransStateT :: MonadTrans (StateT s)  where
  lift :: ∀ m a. Monad m => m a -> StateT s m a
  lift mx = StateT \s -> mx <#> \x -> Tuple x s

instance monadThrowStateT :: MonadThrow e m => MonadThrow e (StateT s m) where
  throwError :: ∀ a . e -> StateT s m a
  throwError  = lift <<< throwError

instance monadErrorStateT :: MonadError e m => MonadError e (StateT s m) where
  catchError :: ∀ a. StateT s m a -> (e -> StateT s m a) -> StateT s m a
  catchError (StateT fmx) fm = StateT \s ->
                    catchError (fmx s) \e ->  runStateT (fm e) s

test :: Effect Unit
test = do
  log "Placeholder Ch21"