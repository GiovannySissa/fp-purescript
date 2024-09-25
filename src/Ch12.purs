module Ch12 where

import Prelude

import Effect (Effect)
import Effect.Console (log)


data Maybe a = Nothing | Just a
instance functorMaybe  :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x


-- f:: ∀ a b. a -> b
-- g:: ∀ b c. b -> c
-- h:: ∀ c d. c -> d 

-- hgf :: ∀ a d. a -> d
-- hgf = h <<< g <<< f

-- f is a Functor
class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$>

test :: Effect Unit
test = do
  log "CH 12" 