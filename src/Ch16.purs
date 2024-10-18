module Ch16 where

import Prelude (Unit, (<$>), ($), (*), (<>), show, discard)
import Effect (Effect)
import Effect.Console (log)
import Data.Functor (class Functor)

import Data.Either (Either(..))

import Data.List (List(..), (:))
import Data.Generic.Rep (class Generic)
import Data.Monoid (class Monoid, mempty)
import Data.Semigroup (class Semigroup)
import Data.Tuple (Tuple(..))
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.String (toLower)


data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

-- use case Value is on the oustide and the Function is inside

applyToMaybe :: ∀ a b. Maybe(a -> b) -> Maybe a -> Maybe b
applyToMaybe Nothing _ = Nothing
applyToMaybe (Just f) x = f <$> x


-- In order to follow the pattern there is Typeclass called Apply

class Functor f <= Apply f where
  apply :: ∀ a b. f (a -> b) -> f a -> f b

infixl 4 apply as <*>

class Apply f <= Applicative f where 
  pure :: ∀ a. a -> f a

{-
instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance applyMaybe :: Apply Maybe where
  apply Nothing _ = Nothing
  apply (Just f) x = f <$> x

instance applicativeMaybe :: Applicative Maybe where
  pure = Just 

since we can write Apply in terms of map

pure f <*> x = f <$> x
  -}

instance applicativeMaybe :: Applicative Maybe where
  pure = Just 

instance functorMaybe :: Functor Maybe where
  -- map = liftA1  where liftA1 := pure f <*> x
  map f x =  pure f <*> x

instance applyMaybe :: Apply Maybe where
  apply (Just f) (Just x) = Just (f x)
  apply Nothing _ = Nothing
  apply _ Nothing = Nothing


instance applyTuple :: Semigroup a => Apply (Tuple a) where
  apply (Tuple x f)(Tuple y z) = Tuple (x <> y) (f z) 

instance applicativeTuple :: Monoid a => Applicative (Tuple a) where
  pure x = Tuple mempty x

data Threeple a b c = Threeple a b c

instance functorThreeple:: Functor (Threeple a b) where
  map f (Threeple x y z) = Threeple x y (f z)

instance applyThreeple :: (Semigroup a, Semigroup b) => Apply (Threeple a b) where
  apply (Threeple u v f) (Threeple x y z) = Threeple(u <> x) (v <> y) (f z)


instance applicativeThreeple :: (Monoid a, Monoid b ) => Applicative (Threeple a b) where
  pure x = Threeple mempty mempty x

derive instance genericThreeple :: Generic (Threeple a b c) _

instance showThreeple :: (Show a, Show b, Show c) =>   Show (Threeple a b c) where
  show = genericShow
  

threeple :: ∀ a b c f. Applicative f => f a -> f b -> f c -> f (Threeple a b c)
threeple fx fy fz = pure Threeple <*> fx <*> fy <*> fz

{-
  Sometimes we have  a sum type the implementation is fairly simple just apply the wrap right hand type
  but not always is the same, for example
-}

data Things a b c = Thing1 a | Thing2 b c

instance functorThings :: Functor (Things a b) where
  map _ (Thing1 x)   = Thing1 x
  map f (Thing2 x y) = Thing2 x $f y

instance applyThings :: (Semigroup a, Semigroup b) => Apply (Things a b) where
  apply (Thing2 x f) (Thing2 y z) = Thing2 (x <> y) $f z
  apply (Thing1 f) _ = Thing1 f
  apply _ (Thing1 x) = Thing1 x

instance applicativeThings :: (Monoid a, Monoid b) => Applicative (Things a b) where
  pure x = Thing2 mempty x

testApplyToMaybe :: Effect Unit
testApplyToMaybe = do
  log "======================"
  log $ show $ applyToMaybe (Just (_ * 2)) (Just 100)
  log $ show $ Just(_ * 2) <*> Just(20)
  log $ show $ threeple (Just 20) (Just "aaa") (Just 1.0)  
  log "======================"
  
testApplicativeProductType :: Effect Unit
testApplicativeProductType = do
  log "============tuple =============="
  log $ show $ Tuple "abc" (_ * 10) <*> Tuple "123" 42
  log $ show $ pure (_ * 10) <*> Tuple "abc" 42
  log $ show $ Tuple "abc" (_ * 10) <*> pure 42  
  log "========== Threeple ============"
  log $ show $ Threeple "abc" (1: Nil) (_ * 10) <*> Threeple "123" (2: Nil) 80
  log $ show $ Threeple ("1.0") (2: Nil) (toLower)  <*> pure "UPPER CASE" 

-- 16.8 

-- instance functorEither :: Functor (Either a) where
--   map _ (Left x) = Left x
--   map f (Right x) = Right $ f x

instance applyEither :: Apply (Either a) where
  apply (Right f) (Right x) = Right $ f x
  apply (Left y) _ = Left y
  apply _ (Left y)  = Left y

-- assumes non nullable values
fullName0 :: String -> String -> String -> String
fullName0 first middle last = first <> " " <> middle <> " " <> last

-- it's gonna fail in case any of arguments too verbose
fullName1 :: Maybe String -> Maybe String -> Maybe String -> Maybe String
fullName1 (Just first) (Just middle) (Just last) = Just $ first <> " " <> middle <> " " <> last
fullName1 Nothing _ _ = Nothing
fullName1 _ Nothing _ = Nothing
fullName1 _ _ Nothing = Nothing

-- too verbose but we have the error part
fullName2 :: Maybe String -> Maybe String -> Maybe String -> (Either String String)
fullName2 (Just first) (Just middle) (Just last) = Right $ first <> " " <> middle <> " " <> last
fullName2 Nothing _ _ = Left "Missing first name"
fullName2 _ Nothing _ = Left "Missing middle name"
fullName2 _ _ Nothing = Left "Missing last name"

-- better approach
fullName :: String -> String -> String -> String
fullName first middle last = first <> " " <> middle <> " " <> last

fullNameEither :: Maybe String -> Maybe String -> Maybe String -> (Either String String)
fullNameEither first middle last = 
  fullName <$> (first  `errIfMissing` "First name must exist")
           <*> (middle `errIfMissing` "Middle name must exist")
           <*> (last   `errIfMissing` "Last name must exist")
  

errIfMissing ::  Maybe String -> String -> (Either String String)
errIfMissing Nothing err = Left err
errIfMissing (Just x) _ = Right x







test:: Effect Unit
test = do
   testApplyToMaybe
   testApplicativeProductType