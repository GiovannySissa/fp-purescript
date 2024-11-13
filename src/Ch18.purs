module Ch18 where

import Prelude

import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

import Data.Tuple (Tuple(..))

import Effect (Effect)
import Effect.Console (log)


f' :: Int -> Debuggable Int
f' x = Debuggable(Tuple "Added 10\n" (x + 10))

g' :: Int -> Debuggable Int
g' x = Debuggable(Tuple "Multiply by 100\n" (x * 1000))

-- g' <<< f' 
h :: Int -> Debuggable Int
h z = 
  let Debuggable (Tuple s r) = f' z 
      Debuggable (Tuple s' t) = g' r in
  Debuggable(Tuple (s <> s') t)




{-Let's generalize Deugable-}


-- since we generalize debuggable we will need a newtype instead
-- type Debuggable a = Tuple String a

{-As composeDebuggable and applyDebuggable are similar we migth write one in terms  of the other -}
composeDebuggable 
  :: ∀ b c d
  .  (c -> Debuggable d)
  -> (b -> Debuggable c)
  -> (b -> Debuggable d)
composeDebuggable gc fc x = 
  fc x `applyDebuggable0` gc 
  -- let Tuple s r = fc x
  --     Tuple s' t = gc r in 
  -- Tuple (s <> s') t


-- write h in terms of composeDebuggable

h' :: Int -> Debuggable Int
-- h' z = composeDebuggable g' f' z 
h' = g' `composeDebuggable` f' 

makeFuncDebuggable :: ∀ a b. (a -> b) -> a -> Debuggable b
makeFuncDebuggable fp z = makeDebuggable (fp z)


-- non-side-effect := nse
nse :: Int -> Int
nse x = x + 42

c :: Int -> Debuggable Int
c = makeFuncDebuggable nse `composeDebuggable` f' 

applyDebuggable0 
  :: ∀ a b
  . Debuggable a
  -> (a -> Debuggable b)
  -> Debuggable b 
applyDebuggable0 (Debuggable(Tuple s x)) sef = 
  let Debuggable(Tuple a b) = sef x in
  Debuggable(Tuple (s <> a ) b)

y :: Debuggable Int
y = (12345 # f') `applyDebuggable0` g'

makeDebuggable :: ∀ a. a -> Debuggable a
makeDebuggable x = Debuggable(Tuple "" x)
y' :: Debuggable Int
y' = makeDebuggable 12345 `applyDebuggable0` f'  `applyDebuggable0` g'


class Applicative f <= SideEffect f where
  applySideEffect :: ∀ a b. f a -> (a -> f b) -> f b

newtype Debuggable a = Debuggable(Tuple String a)

derive instance newtypeDebuggable:: Newtype (Debuggable a) _
derive instance newtypeGeneric:: Generic (Debuggable a) _

instance showDebuggable :: Show a => Show (Debuggable a) where
  show = genericShow

derive newtype instance functorDebuggable :: Functor Debuggable 
-- instance functorDebuggable :: Functor Debuggable where
--   map f (Debuggable(Tuple s x) ) = Debuggable(Tuple s $ f x)

derive newtype instance applyDebuggable :: Apply Debuggable 
-- instance applyDebuggable :: Apply Debuggable where
--   apply :: ∀ a b. (Debuggable (a -> b)) -> Debuggable a -> Debuggable b
--   apply (Debuggable(Tuple s f)) (Debuggable s' x) =
--     Debuggable (Tuple (s <> s') $f x ) 
  

derive newtype instance applicativeDebuggable :: Applicative Debuggable
-- instance applicativeDebuggable :: Applicative Debuggable where
--   pure a = Debuggable(Tuple "" a) 

instance sideEffectDebuggable :: SideEffect Debuggable where
  applySideEffect :: ∀ a b. Debuggable a -> (a -> Debuggable b) -> Debuggable b
  applySideEffect (Debuggable (Tuple s x)) sef = 
    let Debuggable (Tuple a b) = sef x in
    Debuggable(Tuple (s <> a ) b )


-- Countable
newtype Count = Count Int
derive instance newtypeCount :: Newtype Count _
derive instance genericCount :: Generic Count _
instance showCount :: Show Count where
  show = genericShow

derive newtype instance semiringCount :: Semiring Count
instance monoidCount :: Monoid Count where
  mempty = zero
 
instance semigroupCount :: Semigroup Count where
  append = (+)

newtype Countable a = Countable(Tuple Count a)
derive instance newtypeCountable :: Newtype (Countable a) _
derive instance genericCountable :: Generic (Countable a) _
instance showCountable :: Show a => Show (Countable a) where
  show = genericShow

derive newtype instance functorCountable :: Functor Countable 
derive newtype instance applyCountable :: Apply Countable 
derive newtype instance applicativeCountable :: Applicative Countable 

instance sideEffectCountable :: SideEffect Countable where
  applySideEffect :: ∀ a b. Countable a -> (a -> Countable b) -> Countable b
  applySideEffect (Countable (Tuple s x)) sef = 
    let Countable (Tuple a b) = sef x in
    Countable(Tuple (s + a) b )


{-
 composing side effect functions
-}
compose 
  :: ∀ b c d 
  .  (c -> d)
  -> (b -> c)
  -> (b -> d)
compose g f x = g $ f x

composeKleisli 
  :: ∀ b c d m  
  . Monad m =>
     (c -> m d)
  -> (b -> m c)
  -> (b -> m d)
composeKleisli g f x = g =<< f x


-- testing

simpleTest :: Effect Unit
simpleTest = do 
  log $ show $ g' 10
  log $ show $ f' 50
  log $ show $ h 20
  log $ show $ h' 20
  log $ show $ c 5
  log $ show $ y
  log $ show $ y'


test:: Effect Unit
test = do
  log "Monads test!"
  simpleTest