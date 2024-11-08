module Ch18 where

import Prelude

import Data.Tuple (Tuple(..))

import Effect (Effect)
import Effect.Console (log)


f :: Int -> Debuggable Int
f x = Tuple "Added 10\n" (x + 10)

g :: Int -> Debuggable Int
g x = Tuple "Multiply by 100\n" (x * 1000)

-- g <<< f 
h :: Int -> Debuggable Int
h z = 
  let Tuple s r = f z 
      Tuple s' t = g r in
  Tuple (s <> s') t



{-
 composing side effect functions
-}
-- compose 
--   :: ∀ b c d 
--   .  (c -> d)
--   -> (b -> c)
--   -> (b -> d)
type Debuggable a = Tuple String a

{-As composeDebuggable and applyDebuggable are similar we migth write one in terms  of the other -}
composeDebuggable 
  :: ∀ b c d
  .  (c -> Debuggable d)
  -> (b -> Debuggable c)
  -> (b -> Debuggable d)
composeDebuggable gc fc x = 
  fc x `applyDebuggable` gc 
  -- let Tuple s r = fc x
  --     Tuple s' t = gc r in 
  -- Tuple (s <> s') t


-- write h in terms of composeDebuggable

h' :: Int -> Debuggable Int
-- h' z = composeDebuggable g f z 
h' = g `composeDebuggable` f 

makeFuncDebuggable :: ∀ a b. (a -> b) -> a -> Debuggable b
makeFuncDebuggable fp z = makeDebuggable (fp z)


-- non-side-effect := nse
nse :: Int -> Int
nse x = x + 42

c :: Int -> Debuggable Int
c = makeFuncDebuggable nse `composeDebuggable` f 

applyDebuggable 
  :: ∀ a b
  . Debuggable a
  -> (a -> Debuggable b)
  -> Debuggable b 
applyDebuggable (Tuple s x) sef = 
  let Tuple a b = sef x in
  Tuple (s <> a ) b  

y :: Debuggable Int
y = (12345 # f) `applyDebuggable` g

makeDebuggable :: ∀ a. a -> Debuggable a
makeDebuggable x = Tuple "" x
y' :: Debuggable Int
y' = makeDebuggable 12345 `applyDebuggable` f  `applyDebuggable` g


-- testing

simpleTest :: Effect Unit
simpleTest = do 
  log $ show $ g 10
  log $ show $ f 50
  log $ show $ h 20
  log $ show $ h' 20
  log $ show $ c 5
  log $ show $ y
  log $ show $ y'


test:: Effect Unit
test = do
  log "Monads test!"
  simpleTest