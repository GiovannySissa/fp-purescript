module Ch17 
(
  Age(..),
  FamilyAges(..),
  FamilyAgesRow,
  Validation(..),
  createFamilyAges,
  test,
  Either(..)
)
where

import Prelude 

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Bifunctor (class Bifunctor)
import Data.Newtype (class Newtype)

import Effect (Effect)
import Effect.Console (log)


-- Applicative instance for maybe
data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x 

instance applyMaybe :: Apply Maybe where
  apply (Just f) x = f <$> x
  apply Nothing _ = Nothing

instance applicativeMaybe :: Applicative Maybe where
  pure = Just

testMaybeInstances :: Effect Unit
testMaybeInstances = do
  log "##### Maybe Instances test ########" 
  log $ show $ (+) <$> Just 21 <*> Just 21
  log $ show $ (*) <$> pure 2 <*> (pure 21 :: Maybe Int)
  log $ show $ pure (+) <*> Just 17 <*> Just 25
  log "###################################"
  log "" 

-- Either instances

data Either a b = Left a | Right b

derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b) 
derive instance functorEither :: Functor (Either a) 

derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance bifunctorEither :: Bifunctor Either where
  bimap g _ (Left x) = Left $ g x
  bimap _ f (Right x) = Right $ f x

instance applyEither :: Apply (Either a) where
  apply (Right f) x = f <$> x
  apply (Left err) _ = Left err

instance applicativeEither :: Applicative (Either a) where
  pure x = Right x

testEitherInstances :: Effect Unit
testEitherInstances = do
  log "##### Either Instances test ########" 
  let a = pure 1 :: Either Unit Int
  log $ show $ (pure identity <*> pure identity <*> a) == (pure identity <*> (pure identity <*> a))
  log $ show $ (pure identity <*>  a) == a 
  log $ show $ pure (negate 1) == (pure negate <*> a)
  log $ show $ (pure negate <*> a) == (pure (_ $ 1) <*> pure negate )
  log "###################################"
  log "" 

-- Validation implementation

newtype Validation err result = Validation (Either err result)

derive instance eqValidation :: (Eq err, Eq result) => Eq (Validation err result)
derive instance ordValidation :: (Ord err, Ord result) => Ord (Validation err result)
derive instance genericValidation :: Generic(Validation err result) _
instance showValidation :: (Show err, Show result) => Show (Validation err result) where 
  show = genericShow
derive instance newtypeValidation :: Newtype (Validation err result) _
derive newtype instance functorValidation :: Functor (Validation err)
derive newtype instance bifunctorValidation :: Bifunctor (Validation)



instance applyValidation :: Semigroup err => Apply (Validation err) where
  apply (Validation(Left err1)) (Validation(Left err2)) = Validation $Left (err1 <> err2)
  apply (Validation(Left accErr)) _ = Validation (Left accErr)
  apply (Validation (Right f)) x = f <$> x

instance applicativeValidation :: Semigroup err => Applicative (Validation err) where
  pure = Validation <<< Right

-- Using validation

newtype Age = Age Int
derive instance genericAge:: Generic Age _
instance showAge :: Show Age where
  show = genericShow
newtype FullName = FullName String
derive instance genericFullName:: Generic FullName _
instance showFullName :: Show FullName where
  show = genericShow
type FamilyAgesRow r = (fatherAge :: Age, motherAge :: Age, childAge :: Age | r)

type FamilyNamesRow r = (fatherName :: FullName, motherName :: FullName, childName :: FullName | r)
newtype Family = Family{ | FamilyNamesRow(FamilyAgesRow()) }
derive instance genericFamily :: Generic Family _
instance showFamily :: Show Family where
  show = genericShow

newtype FamilyAges = FamilyAges { | FamilyAgesRow() }
derive instance genericFamilyAges :: Generic FamilyAges _
instance showFamilyAges :: Show FamilyAges where
  show = genericShow


newtype UpperAge = UpperAge Int
newtype LowerAge = LowerAge Int
validateAge 
  :: LowerAge 
  -> UpperAge 
  -> Age 
  -> String 
  -> Validation (Array String) Age
validateAge (LowerAge lower) (UpperAge upper) (Age age) who 
  | age < lower = Validation $ Left [who <> " is too young"]
  | age > upper = Validation $ Left [who <> " is too older"]
  | otherwise   = Validation $ Right $ Age age



{-
We need a function to do this
  \n fa, ma, ca -> {fatherAge: fa, motherAge: ma, childAge: ca}

-}
createFamilyAges :: { | FamilyAgesRow ()} -> Validation (Array String) FamilyAges
createFamilyAges {fatherAge, motherAge, childAge} = 
  FamilyAges <$> ({ fatherAge: _, motherAge: _, childAge: _}
    <$> (validateAge (LowerAge 18) (UpperAge 100) fatherAge "Father")
    <*> (validateAge (LowerAge 18) (UpperAge 100) motherAge "Mother")
    <*> (validateAge (LowerAge 1) (UpperAge 18) childAge "Child"))


testFamilyAges:: Effect Unit
testFamilyAges = do 
  log "***** Family Ages Test   ******"
  log $ show $ createFamilyAges {fatherAge: Age 40, motherAge: Age 30, childAge: Age 10}
  log $ show $ createFamilyAges {fatherAge: Age 400, motherAge: Age 300, childAge: Age 0}
  log $ show $ createFamilyAges {fatherAge: Age 4, motherAge: Age 3, childAge: Age 10}
  log $ show $ createFamilyAges {fatherAge: Age 40, motherAge: Age 30, childAge: Age 100}  
  log $ show $ createFamilyAges {fatherAge: Age 40, motherAge: Age 3, childAge: Age 0}  
  log "*******************************"
  log ""

test :: Effect Unit
test = do
  testMaybeInstances
  testEitherInstances
  testFamilyAges
  