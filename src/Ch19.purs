module Ch19 where

import Prelude

import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
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

test :: Effect Unit
test = do
  log "CH 19 Placeholder"
  maybeTest
  eitherTest