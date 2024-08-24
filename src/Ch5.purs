module Ch5 where

import Prelude (Unit, (+), show, discard)

import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)


flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip fn x y = fn y x 

flip' :: ∀ a b c. (a -> b -> c) -> (b -> a -> c)
flip' fn = \x y -> fn y x

flip'' :: ∀ a b c. (a -> b -> c) -> b -> (a -> c)
flip'' f x = \y -> f y x


{-
  Implement the  `const` function 
-}

const :: ∀ a b. a -> b -> a 
const x _ =  x 

const' :: ∀ a b. a -> b -> a 
const' x = \_ -> x 


apply :: ∀ a b. (a -> b) ->  a ->  b
apply f x = f x

infixr 0 apply as $ 

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply
-- applyFlipped x f = f x

infixl 1 applyFlipped as #


{-
  Section 5.10 to 5.15
-}
singleton :: ∀ a. a -> List a
singleton x = x : Nil
-- singleton v = Cons v Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

snoc :: ∀ a. List a -> a -> List a
snoc Nil x = singleton x
snoc (x : xs) x1 = x : snoc xs x1


length :: ∀ a. List a -> Int
length Nil = 0
length (_: xs) = 1 + length(xs)

-- uses tail recursion
{-
  This version isn't safe since the first call can 
  include incorrect data e.g length' 1 Nil will produce incorrect data
-}
length' :: ∀ a. Int -> List a -> Int
length' acc Nil = acc
length' acc (_: xs) = length' (1 + acc) xs

length'' :: ∀ a. List a -> Int 
length'' l = go 0 l where
  go :: Int -> List a -> Int
  go acc Nil = acc
  go acc (_: xs) = go (1 + acc) xs




test :: Effect Unit
test = do
  -- log (show (flip const 1 2))
  -- log (show (flip' const 1 2))
  -- log $ show $ flip const 1 2
  -- flip const 1 2 # show # log
  -- log $ show $null Nil
  -- log $ show $null ("xyz" : Nil)
  log $ show $ snoc (1 : 2 : Nil) 3
  log $ show $ length $ 1 : 2 : 3 : Nil
  log $ show $ length'' $ 1 : 2 : 3 : Nil
