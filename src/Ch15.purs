module Ch15 where

import Prelude

import Data.Int.Bits ((.&.))
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Foldable (class Foldable, foldl)
import Data.List (List(..), (:))
import Data.Profunctor (class Profunctor, dimap)
import Data.String (length)
import Effect (Effect)
import Effect.Console (log)


even :: Int -> Boolean
even x = x .&. 1 == 0

odd :: Int -> Boolean
odd x = x .&. 1 == 1

testOdd :: Effect Unit
testOdd = do
  log "==========ODD test==============="
  log $ show $ odd 0
  log $ show $ odd 1
  log "================================="

data Predicate a = Predicate (a -> Boolean)

runPredicate :: ∀ a. Predicate a -> a -> Boolean
runPredicate (Predicate f) x = f x

testRunPredicate:: Effect Unit
testRunPredicate = do
  log "==========RunPredicate test==============="
  log $ show $ runPredicate (Predicate(odd)) $ 10
  log $ show $ runPredicate (Predicate(odd)) $ 11
  log "=========================================="

testContravariant :: Effect Unit
testContravariant = do
  log "=========Contravariant RunPredicate============"
  log $ show $ runPredicate (cmap (_ + 1) (Predicate odd)) 10
  log $ show $ runPredicate (cmap (_ + 2) (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 1) >$< (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 2) >$< (Predicate odd)) 10
  log "==============================================="

instance contravariantPredicate :: Contravariant Predicate where
  cmap fc (Predicate p) =  Predicate(fc >>> p)


-- moore machines
data Moore s a b = Moore s (s -> b) (s -> a -> s)

data OvenState = Off | Bake | Idling -- state
data Heat = HeatOn | HeatOff -- output

-- transition function
data InputSignal = BakePressed | OffPressed | TooHot | TooCold -- input

transitionFn :: OvenState -> InputSignal -> OvenState
transitionFn Off BakePressed = Bake
transitionFn Bake OffPressed = Off
transitionFn Bake TooHot = Idling
transitionFn Idling TooCold = Bake
transitionFn Off OffPressed = Off
transitionFn s _ = s


outputFn :: OvenState -> Heat
outputFn Off = HeatOff
outputFn Bake = HeatOn
outputFn Idling = HeatOff

{-
  Moore s a b 
  := Moore s (s -> b) (s -> a -> s)
  Moore s c d 
  := Moore s (s -> d) (s -> c -> s)


  --types definition
  contravariant :: c -> a
  covariant     :: b -> d
  transition s ::  a -> s 
  transition' s :: c -> s
  output :: s -> b
  covariant <<< output :: (s -> b) -> (b -> d) == s -> d
-}

instance profunctorMoore:: Profunctor (Moore s) where
  dimap :: ∀ a b c d. (c -> a) -> (b -> d) -> Moore s a b -> Moore s c d
  dimap contravariant covariant (Moore s0 output transition) = Moore s0 (covariant <<< output ) (\s -> transition s <<< contravariant)

-- folds with a Moore machine
-- Moore s a b
-- addr :: Moore Int Int Int
-- addr = Moore 0 identity (+)
addr :: ∀ a. Semiring a => Moore a a a
addr  = Moore zero identity (+)

{-
  output:: s -> b 
  transition :: s -> a -> s
  foldl :: ∀ a b. (b -> a -> b) -> b -> f a -> b
-}
runFoldL :: ∀ f s a b. Foldable f => Moore s a b -> f a -> b
-- runFoldL (Moore s0 output transition) f = output $ foldl transition s0 f 
-- runFoldL (Moore s0 output transition)  = output <<< foldl transition s0 
runFoldL (Moore s0 output transition)  = foldl transition s0 >>> output

testRunFoldL :: Effect Unit
testRunFoldL = do
  log "===========RunFoldL============"
  log $show $runFoldL addr [1, 2, 3]
  log $show $runFoldL addr (1.0 : 2.0 : 3.0 : Nil)
  log "===========++++++++============"

{-
  dimap :: ∀ a b c d. (b -> a) -> () -> f a c -> f b d
-}
sizer :: Moore Int String String
sizer = dimap length (\n -> "Size is " <> show n ) addr

testSizer :: Effect Unit 
testSizer = do 
  log "============Sizer============="
  log $ show $runFoldL sizer ["This", "is", "the", "test"]
  log "============-----============="


test :: Effect Unit
test = do
  testOdd
  testRunPredicate
  testContravariant
  testRunFoldL
  testSizer



