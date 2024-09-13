module Ch7a where

import Prelude (Unit, show, discard, (==), ($), (>), (<), (<=), (>=))

import Data.Eq  (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Ord (class Ord)
import Data.Show (class Show) 
import Effect (Effect)
import Effect.Console (log)

data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show(Maybe a) where
  show = genericShow

derive instance eqMaybe :: Eq a => Eq (Maybe a)
derive instance ordMaybe :: Ord a => Ord (Maybe a)

-- instance eqMaybe :: Eq a => Eq (Maybe a) where
--   eq Nothing Nothing = true
--   eq (Just a) (Just b) = a == b
--   eq _ _ = false
  

-- instance ordMaybe :: Ord a => Ord (Maybe a) where
--   compare Nothing Nothing = EQ
--   compare (Just x) (Just y) = compare x y
--   compare _ Nothing = GT
--   compare Nothing _ = LT 

-- greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
-- greaterThanOrEq x y = 
--   -- comp == EQ || comp == GT where comp = compare x y 
--   let ord = compare x y in
--   case ord of
--     LT -> false
--     _ -> true
  
-- infixl 4 greaterThanOrEq as >=


-- instance showMaybe :: Show a => Show (Maybe a) where
--   show Nothing = "Nothing"
--   show (Just x) = "(Just " <> show x <>")"

{-- Instances for Either
--}

data Either a b = Left a | Right b

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)

derive instance genericEither :: Generic (Either a b) _

instance showEither :: (Show a, Show b) => Show(Either a b) where
  show = genericShow

type MyEitherVar = Either String (Maybe Int)



test:: Effect Unit
test = do
  log "--------Eq--------"
  log " --------Maybe Eq--------"
  log $show $ Just 5 == Just 5
  log $show $ Just 5 == Just 2
  log $show $ Just 5 == Nothing
  log $show $ Nothing == Just 5
  log $show $ Nothing == (Nothing :: Maybe Unit)
  log "--------Maybe Ord-------"
  log $show $ Just 1 < Just 5
  log $show $ Just 5 <= Just 5
  log $show $ Just 5 > Just 10
  log $show $ Just 10 >= Just 10
  log $show $ Just 99 > Nothing
  log $show $ Just 99 < Nothing
  log "-------Maybe Show------"
  log $show $ Just "abc"
  log $show $ (Nothing :: Maybe Unit)
  log "-------Either Show------"
  log $show $ (Left "left" :: Either _ Unit)
  log $show $ (Right (Just 1) :: Either Unit _)
  let x = Left "left" :: MyEitherVar
      y:: MyEitherVar
      y = Right $ Just 1 :: MyEitherVar
  log $show $ x
  log $show $ y

  


