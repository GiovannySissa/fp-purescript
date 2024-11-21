module Ch19 where

import Prelude
-- import Ch19a (Writer(..))

import Data.Tuple (Tuple(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)


-- Writing Monad for Maybe
data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic(Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance applyMaybe :: Apply Maybe where
  apply Nothing _ = Nothing
  apply (Just f) x = f <$> x

instance applicativeMaybe :: Applicative Maybe where 
  pure x = Just x

instance bindMaybe :: Bind Maybe where
  bind Nothing _ = Nothing
  bind (Just x) mf = mf x

instance monadMaybe :: Monad Maybe  

-- Writing Monad Either

data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance functorEither :: Functor (Either a) where
  map _ (Left x) = Left x
  map f (Right x) = Right $ f x

instance applyEither :: Apply (Either a) where
  apply (Left x) _ = Left x
  apply (Right f) x = f <$> x

instance applicativeEither :: Applicative (Either a) where
  pure x = Right x

instance bindEither :: Bind (Either a) where
  bind (Left x) _ = Left x
  bind (Right x) f = f x

instance monadEither :: Monad (Either a)

-- RWS monad

-- newtype Reader r a = Reader (r -> a)
-- newtype Writer w a = Writer (Tuple a w)
-- newtype State s a = State ( s -> Tuple a s)

type RWSResult r w s = {
  r :: r,
  w :: w,
  s :: s
}

newtype RWS r w s a = 
  RWS (RWSResult r w s -> Tuple a (RWSResult r w s))
{-
  g :: RWSResult r w s -> Tuple a (RWSResult r w s)
  rws :: RWSResult r w s 
  g rsw :: Tuple a (RWSResult r w s)
  f :: a -> b 
-}
instance functorRWS :: Functor (RWS r w s) where  
  map f (RWS g) = 
    RWS \rws -> g rws
      # \(Tuple a rsw') -> Tuple (f a) rsw'

instance applyRWS ::Monoid w => Apply (RWS r w s) where
  apply = ap

instance applicativeRWS :: Monoid w => Applicative (RWS r w s) where
  -- pure x = RWS \rws ->  Tuple x rsw
  pure x = RWS \{r, s} ->  Tuple x {r, w: mempty, s }

instance bindRWS :: Monoid w => Bind (RWS r w s) where
  -- bind :: (RWS r w s a) (a -> RWS r w s b) -> (RWS r w s b)
  bind (RWS g) f = RWS \rws -> g rws 
    # \(Tuple x rws'@{w}) -> runRWS (f x) rws'
     # \(Tuple y rws''@{w: w'}) -> Tuple y (rws'' {w = w <> w'})

-- instance bindWriter :: Monoid w => Bind (Writer w) where
--   bind:: ∀ a b. Writer w a -> (a -> Writer w b) -> Writer w b
--   bind (Writer (Tuple x w1)) f  = f x 
--     # \(Writer (Tuple x' w2)) -> Writer (Tuple x' (w1 <> w2))

instance monadRWS :: Monoid w => Monad (RWS r w s) 

runRWS :: ∀ r w s a.
  RWS r w s a 
  -> RWSResult r w s 
  -> Tuple a (RWSResult r w s)
runRWS (RWS f) = f

-- RWS api
-- "pushes" data from pure computation to monadic
tell :: ∀ r w s. w -> RWS r w s Unit
tell w = RWS \{r, s} -> Tuple unit {r, w, s}

-- pulls data from monadic computation to pure
ask  :: ∀ r w s. Monoid w => RWS r w s r
ask = RWS \ {r, s} -> Tuple r {r, w: mempty, s}

get  :: ∀ r w s. Monoid w => RWS r w s s
get = RWS \ {r, s} -> Tuple s {r, w: mempty, s}
-- push
put :: ∀ r w s. Monoid w => s -> RWS r w s Unit
put s = RWS \ {r} -> Tuple unit {r, w: mempty, s}
-- Testing 
maybeTest :: Effect Unit
maybeTest = do 
  log "Maybe Test"
  log $ show $ Just (_ * 10) <*> Just 20
  log $ show $ Just (_ * 10) <*> pure 20
  log $ show $ Just 20 >>= pure <<< (_ * 10)
  log $ show do
    x <- Just 20
    let y = x * 10
    pure y
  log $ show $ Just 20 >>= const Nothing >>= \y -> Just $ y + 42
  log $ show do
    _ <- Just 20
    y <- Nothing
    pure $ y + 54
  log ""

eitherTest :: Effect Unit
eitherTest = do
  log "Either Test"
  log $ show $ Right (_ * 10) <*> (Right 20 :: Either Unit _)
  log $ show $ Right (_ * 10) <*> (pure 20  ::Either Unit _)
  log $ show $ (Right 20 :: Either Unit _) >>= pure <<< (_ * 10)
  log $ show do
    x <- (Right 20 :: Either Unit _)
    let y = x * 10
    pure y
  log $ show $ Right 20 >>= const Left "error" >>= \y -> Right $ y + 42
  log $ show do
    _ <- Right 20
    y <- Left "Error"
    pure $ y + 54
  log ""

type Config = { debugModeOn :: Boolean }

type Counter = Int

rwsExec :: RWS Config (Array String) Counter Unit
rwsExec = do
  tell ["test the log"]
  tell ["test the log2", "test the log3"]
  config <- ask
  tell ["the config is " <> show config]
  counter <- get
  tell ["old counter is " <> show counter]
  put $ counter + 1
  newCounter <- get  
  tell ["new counter is " <> show newCounter]
  pure unit

rwsTest :: Effect Unit
rwsTest = do
  log "RWS Test"
  log $ show $ runRWS rwsExec {r : { debugModeOn : true }, w: mempty, s: 0}
  log ""
test :: Effect Unit
test = do
  log "CH 19 Placeholder"
  maybeTest
  eitherTest
  rwsTest