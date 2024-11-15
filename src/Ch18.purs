module Ch18 where

import Prelude

import Data.Int.Bits ((.&.))
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

composeKleisli' 
  :: ∀ b c d m  
  . Monad m =>
     (c -> m d)
  -> (b -> m c)
  -> (b -> m d)
composeKleisli' g f x = g =<< f x

-- For reference Bind and Monad typeclass

class Apply m <= Bind0 m where
  bind :: ∀ a b. m a -> (a -> m b) -> m b

join :: ∀ a m. Bind0 m => m (m a) -> m a
join mma = mma >>= identity

infixl 1 bind as >>=

class (Applicative m, Bind0 m) <= Monad0 m

-- Monad instance for Maybe

data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show(Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance applyMaybe :: Apply Maybe where
  apply :: ∀ a b. Maybe(a -> b) -> Maybe a -> Maybe b
  -- apply Nothing _ = Nothing
  -- apply (Just f) x = f <$> x
  -- apply mf mx = mf >>= \f -> mx >>= pure <<< f
  apply = ap

ap :: ∀ a b m. Monad0 m => m (a -> b) -> m a -> m b
ap mf mx = do 
    f <- mf 
    x <- mx
    pure $ f x

instance applicativeMaybe :: Applicative Maybe where
  pure = Just

instance bindMaybe :: Bind0 Maybe where
  bind Nothing _ = Nothing
  bind (Just x) mf = mf x

instance monad0Maybe :: Monad0 Maybe

-- Monad instance for Either0
data Either0 a b = Left a | Right b

derive instance genericEither0 :: Generic (Either0 a b) _
instance showEiher0 :: (Show a, Show b) => Show (Either0 a b) where
  show = genericShow

instance functorEither0 :: Functor(Either0 l) where
  map _ (Left x) = Left x
  map f (Right x) = Right $ f x

instance applyEither0 :: Apply(Either0 l) where
  -- apply (Left x) _  = Left x
  -- apply (Right f) x = f <$> x
  apply = ap

instance applicativeEither0 :: Applicative(Either0 l) where
  pure = Right

instance bindEither0 :: Bind0(Either0 l) where
  bind (Left x) _ = Left x
  bind (Right x) mf = mf x

instance monad0Either0 :: Monad0 (Either0 l)


-- more realistic test for either

fullName :: String -> String -> String -> String
fullName first middle last = first <> " " <> middle <> " " <> last
errIfMissing ::  Maybe String -> String -> (Either0 String String)
errIfMissing Nothing err = Left err
errIfMissing (Just x) _ = Right x

fullNameEither :: Maybe String -> Maybe String -> Maybe String -> (Either0 String String)
fullNameEither fst mid lst = do
  first  <- errIfMissing fst "First name must exist"
  middle <- mid `errIfMissing` "Middle name must exist"
  last   <- lst `errIfMissing` "Last name must exist"
  pure $ fullName first middle last

-- Writer monad 
newtype Writer w a = Writer (Tuple a w)
derive  instance newtypeWriter :: Newtype(Writer w a) _

derive instance genericWriter :: Generic(Writer w a) _

instance showWriter :: (Show w, Show a) => Show (Writer w a) where
  show = genericShow


instance functorWriter :: Functor(Writer a) where
  map f (Writer(Tuple x l)) = Writer (Tuple (f $ x) l)

instance applyWriter :: Monoid a => Apply (Writer a) where
  apply = ap
  -- apply (Writer (Tuple f e)) (Writer(Tuple a e1)) = 
  --   Writer(Tuple (f a) (e <> e1))

instance applicativeWriter :: Monoid a => Applicative (Writer a) where
  pure x = Writer(Tuple x mempty)

instance bindWriter :: Monoid w => Bind0 (Writer w) where
  bind (Writer (Tuple a e)) mf = 
    mf a # \(Writer (Tuple a1 e1)) -> Writer(Tuple a1 (e <> e1))

instance monadWriter :: Monoid w => Monad0(Writer w)

-- writer api (helper functions)
tell :: ∀ a w. w -> Writer w Unit
tell w = Writer (Tuple unit w)
runWriter :: ∀ a s. Writer s a ->  Tuple a s
runWriter (Writer f) = f 

-- Reader Monad 
type Config = 
  {
    numDecimalPlaces :: Int,
    debug :: Boolean
  }
newtype Reader r a = Reader(r -> a)

runReader :: ∀ a r. Reader r a -> r -> a
runReader (Reader f) = f

derive instance newtypeReader :: Newtype (Reader r a) _
derive instance genericReader :: Generic(Reader r a) _ 
-- instance showReader :: (Show r, Show a) => Show (Reader r a) where
--   show = genericShow 

instance functorInstance :: Functor (Reader r) where
  map f (Reader g) = Reader \r1 -> f $g r1

instance applyReader :: Apply(Reader r) where
  apply:: ∀ a b. (Reader r (a->b)) -> (Reader r  a) -> (Reader r b)
  apply (Reader ff)(Reader fx) = Reader \r -> ff r $ fx r

instance applicativeReader :: Applicative(Reader r) where 
  pure = Reader <<< const 

instance bindReader :: Bind0 (Reader r) where
  bind :: ∀ a b. Reader r a -> (a -> Reader r b) -> Reader r b
  bind (Reader fx) f = Reader \r -> runReader(f $ fx r) r
-- funcWithConfig :: Int -> Reader Config Int
-- funcWithConfig v = do
--   { numDecimalPlaces } <- ask 
instance monad0Reader :: Monad0 (Reader r)
-- reader API
ask :: ∀ r. Reader r r
ask = Reader identity

asks :: ∀ a r. (r -> a) -> Reader r a
asks f = Reader \r -> f r

-- State monad
newtype State s a = State (s -> Tuple a s)

instance functorState :: Functor (State s) where
  map f (State fs) = State \s -> fs s # \(Tuple x s') -> Tuple (f x) s'

instance applyState :: Apply(State s) where
  apply :: ∀ a b. (State s (a->b)) -> State s a -> State s b 
  apply (State ff) (State fx) = State \s -> ff s 
    # \(Tuple g s') ->  fx s' 
      # \(Tuple a s'') -> Tuple (g a) s''

instance applicativeState :: Applicative (State s) where
  pure x = State \s -> Tuple x s

instance bindState :: Bind0 (State s) where 
  bind :: ∀ a b. State s a -> (a -> State s b) -> State s b
  -- bind (State fx) ff = State \s -> fx s # \(Tuple a s') -> ff a # \(Tuple b s'') -> Tuple b s'' 
  bind (State fx) ff = 
    State \s -> fx s 
      # \(Tuple x s') -> runState(ff x) s' 
        
instance monadState :: Monad0 (State s)
-- State api

get :: ∀ s. State s s
get = State \s -> Tuple s s
put :: ∀ s. s -> State s Unit
put ns = State \_ -> Tuple unit ns 
modify:: ∀ s. (s -> s) -> State s s
modify f = State \s -> let ns = f s in Tuple ns ns
modify_:: ∀ s. (s -> s) -> State s Unit 
modify_ f = State \s -> Tuple unit (f s)

runState :: ∀ a s. State s a -> (s -> Tuple a s) 
runState (State f) = f 

-- using state as validation

errIfMissingS ::  Maybe String -> String -> State (Array String) String
errIfMissingS Nothing err = 
  modify_ (_ <> [err]) >>= \_ -> pure mempty
errIfMissingS (Just x) _ = pure x


fullNameValid :: Maybe String -> Maybe String -> Maybe String -> State (Array String) String
fullNameValid fst mid lst = do
  first  <- errIfMissingS fst "First name must exist"
  middle <- mid `errIfMissingS` "Middle name must exist"
  last   <- lst `errIfMissingS` "Last name must exist"
  pure $ fullName first middle last

-- using writer as validation

errIfMissingW ::  Maybe String -> String -> Writer (Array String) String
errIfMissingW Nothing err = 
  tell [err] >>= \_ -> pure mempty
errIfMissingW (Just x) _ = pure x


fullNameValidW :: Maybe String -> Maybe String -> Maybe String -> Writer (Array String) String
fullNameValidW fst mid lst = do
  first  <- errIfMissingW fst "First name must exist"
  middle <- mid `errIfMissingW` "Middle name must exist"
  last   <- lst `errIfMissingW` "Last name must exist"
  pure $ fullName first middle last


composeKleisli 
  :: ∀ b c d m  
  . Monad0 m =>
     (c -> m d)
  -> (b -> m c)
  -> (b -> m d)
composeKleisli g f = \x -> f x >>= g

infixr 5 composeKleisli as >=>
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
  log ""

oddTest :: Int -> Maybe Int
oddTest x = if x .&. 1 == 1 then Just x else Nothing

greaterThanTest :: Int -> Int -> Maybe Int
greaterThanTest min x = if x > min then Just x else Nothing

lessThanTest :: Int -> Int -> Maybe Int
lessThanTest max x = if x < max then Just x else Nothing

oddTestE :: Int -> Either0 String Int
oddTestE x = if x .&. 1 == 1 then Right x else Left "Number is not odd"

greaterThanTestE :: Int -> Int -> Either0 String Int
greaterThanTestE min x = if x > min then Right x else Left $ "Number isn't greater than " <> show min 

lessThanTestE :: Int -> Int -> Either0 String Int
lessThanTestE max x = if x < max then Right x else Left $ "Number isn't less than " <> show max

gauntlet' :: Int -> Maybe Int
gauntlet' = oddTest >=> pure <<< (_ + 1) >=> greaterThanTest 10 >=> lessThanTest 20

gauntlet'' :: Int -> Maybe Int
gauntlet''  x = 
  pure x >>= oddTest
    >>= \o -> pure (o + 1)
      >>= \y -> greaterThanTest 10 y
        >>= \z -> lessThanTest 20 z

gauntlet :: Int -> Maybe Int
gauntlet x = do 
  o <- oddTest x
  -- y <- pure (o + 1)
  let y = o + 1
  z <- greaterThanTest 10 y
  lessThanTest 20 z

gauntletE :: Int -> Either0 String Int
gauntletE x = do 
  o <- oddTestE x
  -- y <- pure (o + 1)
  let y = o + 1
  z <-  greaterThanTestE 10 y
  lessThanTestE 20 z

fullNameEitherTest :: Effect Unit 
fullNameEitherTest = do 
  log "Fullname Either test"
  log $ show $ fullNameEither (Just "H") (Just "K") (Just "Sm")
  log $ show $ fullNameEither Nothing (Just "K") (Just "Sm")
  log $ show $ fullNameEither (Just "H") Nothing (Just "Sm")
  log $ show $ fullNameEither (Just "H") (Just "K") Nothing
  log ""

doNothingWithLog :: Writer (Array String) Int
doNothingWithLog = 
  tell ["We did nothing"] >>= \_ -> pure 0

maybeMonadTest :: Effect Unit
maybeMonadTest = do 
  log "Maybe monads test"
  log $ show $ gauntlet 14
  log $ show $ gauntlet 1
  log $ show $ gauntlet 93
  log $ show $ gauntlet 17
  log ""

eitherMonadTest :: Effect Unit
eitherMonadTest = do 
  log "Either0 monads test"
  log $ show $ gauntletE 14
  log $ show $ gauntletE 1
  log $ show $ gauntletE 93
  log $ show $ gauntletE 17
  log ""

stateMonadTest :: Effect Unit
stateMonadTest = do 
  log "State monad test"
  log $ show $ runState (fullNameValid Nothing (Just "") Nothing) []
  log ""
  
writerMonadTest :: Effect Unit
writerMonadTest = do 
  log "Writer monad test"
  log $ show $ runWriter (fullNameValidW Nothing (Just "") Nothing)
  log ""
  
test:: Effect Unit
test = do
  log "Monads test!"
  simpleTest
  maybeMonadTest
  eitherMonadTest
  fullNameEitherTest
  log $show $doNothingWithLog
  stateMonadTest
  writerMonadTest