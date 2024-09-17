module Ch9 where

import Prelude (Unit, class Show, class Eq, ($), (==), (&&), discard, show)

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)


class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>

class Semigroup a <= Monoid a where
  mempty :: a


data AndBool = AFalse | ATrue

derive instance eqAndBool :: Eq AndBool
derive instance genericAndBool :: Generic AndBool _
instance showAndBool :: Show AndBool where
  show = genericShow

instance  semigroupAndBool :: Semigroup AndBool where
  append  ATrue ATrue  = ATrue
  append _ _ = AFalse 

instance monoidAndBool :: Monoid AndBool where
  mempty = ATrue


verifyAndBoolSemigroup :: Effect Unit
verifyAndBoolSemigroup = do 
  log "Verifing AndBool Semigroup Laws (1) ∀ a, b, c ∈ S => (a . b ). c = a . (b . c)" 
  log $ show $ (AFalse <> ATrue) <> ATrue == AFalse <> (ATrue <> ATrue)

verifyAndBoolMonoid :: Effect Unit
verifyAndBoolMonoid = do 
  log "Verifing AndBool Monoid Laws (2 tests) ∀ a ∈ S , e ∈ S => (a . e) = (e . a) = a, where mempty is e"
  log $ show $ mempty <> ATrue == ATrue <> mempty && ATrue <> mempty == ATrue
  log $ show $ mempty <> AFalse == AFalse <> mempty && AFalse <> mempty == AFalse

data OrBool = OFalse | OTrue

derive instance eqOrBool :: Eq OrBool
derive instance genericOrBool :: Generic OrBool _

instance showOrBool :: Show OrBool where
  show = genericShow

instance semigroupOrBool :: Semigroup OrBool where
  append OFalse OFalse = OFalse
  append _ _ = OTrue

instance monoidOrBool :: Monoid OrBool where
  mempty = OFalse

sampleOrBool:: Effect Unit
sampleOrBool = do
  log "==== OrBool Examples ===="
  log $show $ OFalse <> OFalse
  log $show $ OTrue  <> OFalse
  log $show $ OFalse <> OTrue

verifyOrBoolSemigroup :: Effect Unit
verifyOrBoolSemigroup = do 
  log "Verifing OrBool Semigroup Laws (1) ∀ a, b, c ∈ S => (a . b ). c = a . (b . c)" 
  log $ show $ (OFalse <> OTrue) <> OTrue == OFalse <> (OTrue <> OTrue)

verifyOrBoolMonoid :: Effect Unit
verifyOrBoolMonoid = do 
  log "Verifing OrBool Monoid Laws (2 tests) ∀ a ∈ S , e ∈ S => (a . e) = (e . a) = a, where mempty is e"
  log $ show $ mempty <> OTrue == OTrue <> mempty && OTrue <> mempty == OTrue
  log $ show $ mempty <> OFalse == OFalse <> mempty && OFalse <> mempty == OFalse


data Mod4 = Zero | One | Two | Three

derive instance eqMod4 :: Eq Mod4
derive instance genericMod4 :: Generic Mod4 _

instance showMod4 :: Show Mod4 where
  show = genericShow


instance semigroupMod4 :: Semigroup Mod4 where
  append Zero other = other
  append other Zero = other
  
  append One One   = Two
  append One Two   = Three
  append One Three = Zero

  append Two One   = Three
  append Two Two   = Zero
  append Two Three = One

  append Three One   = Zero
  append Three Two   = One
  append Three Three = Two


instance monoidMod4 :: Monoid Mod4 where
  mempty = Zero


class Monoid a <= Group a where
  ginverse :: a -> a

instance groupMod4 :: Group Mod4 where
  ginverse Zero  = Zero
  ginverse One   = Three
  ginverse Two   = Two
  ginverse Three = One
  
class Semigroup g <= Conmutative g

instance conmutativeMod4 :: Conmutative Mod4

verifyMod4Semigroup :: Effect Unit
verifyMod4Semigroup = do 
  log "Verifing Mod4 Semigroup Laws (1) ∀ a, b, c ∈ S => (a . b ). c = a . (b . c)" 
  log $ show $ (One <> Two) <> Three == One <> (Two <> Three)

verifyMod4Monoid :: Effect Unit
verifyMod4Monoid = do 
  log "Verifing OrBool Monoid Laws (1 test) ∀ a ∈ S , e ∈ S => (a . e) = (e . a) = a, where mempty is e"
  log $ show $ mempty <> One == One <> mempty && One <> mempty == One


newtype First a = First (Maybe a)

derive instance genericFirst :: Generic (First a) _

instance showFirst :: Show a => Show (First a) where
  show = genericShow

instance semigroupFirst :: Semigroup(First a) where  
  append (First Nothing) other =  other
  append first _ = first

instance monoidFirst :: Monoid (First a) where
  mempty = First Nothing

newtype Last a = Last (Maybe a)

derive instance genericLast :: Generic (Last a) _

instance showLast :: Show a => Show (Last a) where
  show = genericShow

instance semigroupLast :: Semigroup(Last a) where  
  append other (Last Nothing) =  other
  append _ last  = last

instance monoidLast :: Monoid (Last a) where
  mempty = Last Nothing


verifyFirstLast :: Effect Unit
verifyFirstLast = do
  log $ show $ First Nothing <> First(Just 77)
  log $ show $ Last (Just 1) <> Last(Just 99)

test :: Effect Unit
test = do
  log $show $ ATrue  <> ATrue
  log $show $ ATrue  <> AFalse
  log $show $ AFalse <> ATrue
  log "------Monoid ------"
  log $show $ mempty <> ATrue == ATrue
  log $show $ mempty <> AFalse == ATrue
  verifyAndBoolSemigroup
  verifyAndBoolMonoid
  sampleOrBool
  verifyOrBoolSemigroup
  verifyOrBoolMonoid
  verifyMod4Semigroup
  verifyMod4Monoid
  verifyFirstLast

