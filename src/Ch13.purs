module Ch13 where

import Prelude (class Show, Unit, identity, discard, show, ($), (/), (==), (+), (*), (<<<))

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common (toUpper)
import Effect (Effect)
import Effect.Console (log)

class Functor f where
  map:: ∀ a b. (a -> b) -> f a -> f b

 {-
  map identity = identity
  map (g <<< f) = map g <<< map f [Composition]
 -} 

infixl 4 map as <$>

class Bifunctor f where 
  bimap :: ∀ a b c d. (a -> b) -> (c -> d) -> f a c -> f b d

lmap :: ∀ f a b c. Bifunctor f => (a -> b) -> f a c -> f b c
lmap fl = bimap fl identity 

rmap :: ∀ f a b c. Bifunctor f => (b -> c) -> f a b -> f a c
rmap = bimap identity 

data Maybe a = Nothing | Just a


derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

derive instance eqMaybe :: Eq a => Eq (Maybe a)

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

maybeFunctorTest :: Effect Unit
maybeFunctorTest = do
  log "====================================="   
  log "Maybe Functor test"
  log $show $ (_ / 2) <$> Just 10
  log $show $ (_ / 2) <$> Nothing
  log "Maybe Identity for Nothing"
  log $ show $ (identity <$> Nothing) == (Nothing :: Maybe Unit)
  log "Maybe Identity for Just"
  log $ show $ (identity <$> Just [1, 2]) == Just [1, 2]
  log "Maybe Composition"
  let f x = (x * 2)
      g x = (x + 10)
  -- log $ show $ (g <$> ( f <$> Just 10 ) ) == ( g <<< f <$>  Just 10 ) 
  log $ show $ (map (g <<< f) (Just 10)) == (map g <<< map f) (Just 10)
  log "====================================="   


data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance functorEither :: Functor (Either a) where
  map _ (Left x) = Left x
  map f (Right y) = Right (f y)

eitherFunctorTest :: Effect Unit
eitherFunctorTest = do
  log "Either Functor test"
  log $ show $ (_ / 2) <$> (Right 10 :: Either Unit _)
  log $ show $ (_ / 2) <$> (Left "Error reason" :: Either _ Int)


eitherBiFunctorTest :: Effect Unit
eitherBiFunctorTest = do
  log "====================================="   
  log "Either Bifunctor test"
  log $ show $ rmap (_ * 2) $ Left "error reason"
  log $ show $ rmap (_ * 2) $ (Right 10 :: Either Unit _)
  log $ show $ lmap toUpper $ (Left "error reason" :: Either _ Unit)
  log $ show $ lmap toUpper $ (Right 10)
  log "====================================="   



instance bifunctorEither :: Bifunctor Either where
  bimap fl _ (Left a)  = Left $ fl a 
  bimap _ fr (Right a) = Right$ fr a 



tupleFunctorTest :: Effect Unit
tupleFunctorTest = do
  log "====================================="   
  log "Tuple Functor test"
  log $show $(_ / 2) <$> Tuple 10 20
  log "Tuple BiFunctor test"
  log $show $ rmap (_ * 2) $ Tuple 80 40
  log $show $ lmap (_ * 2) $ Tuple 80 40
  log $show $ bimap (_ / 2) (_ * 2) $ Tuple 80 40
  log "====================================="   

data Tuple a b = Tuple a b

derive instance genericTuple :: Generic (Tuple a b) _
instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show = genericShow

instance tupleFunctor :: Functor (Tuple a) where  
  map f (Tuple x y) = Tuple x $f y

instance bifunctorTuple :: Bifunctor Tuple where
  bimap f g (Tuple x y) = Tuple (f x) (g y)

treepleFunctorTest :: Effect Unit
treepleFunctorTest = do
  log "====================================="   
  log "Treeple Functor test"
  log $show $(_ / 2) <$> Treeple 10 20 40
  log "Treeple BiFunctor test"
  log $show $rmap (_ * 2) $ Treeple 99 80 40
  log $show $lmap (_ / 2) $ Treeple 99 80 40
  log $show $bimap (_ / 2) (_ * 2) $ Treeple 99 80 40
  log "====================================="   


data Treeple a b c = Treeple a b c

derive instance genericTreeple :: Generic (Treeple a b c) _
instance showTreple :: (Show a, Show b, Show c) => Show (Treeple a b c) where
  show = genericShow


instance functorTreeple :: Functor (Treeple a b) where
  map f (Treeple x y z) = Treeple x y $ f z

instance bifunctorTreeple :: Bifunctor (Treeple a) where
  bimap f g (Treeple x y z) = Treeple x (f y) (g z)

data Quadruple a b c d  = Quadruple a b c d 

quadrupleFunctorTest :: Effect Unit
quadrupleFunctorTest = do
  log "Quadruple Functor test"
  log $show $(_ / 2) <$> Quadruple 10 20 40 180 

derive instance genericQuadruple :: Generic (Quadruple a b c d) _
instance showQuadruple :: (Show a, Show b, Show c, Show d) => Show (Quadruple a b c d) where
  show = genericShow

instance functorQuadruple :: Functor (Quadruple a b c) where
  map f (Quadruple w x y z) = Quadruple w x y $ f z


test :: Effect Unit
test = do
  maybeFunctorTest
  eitherFunctorTest
  tupleFunctorTest
  treepleFunctorTest
  quadrupleFunctorTest
  eitherBiFunctorTest