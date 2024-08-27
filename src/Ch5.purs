module Ch5 where

import Prelude (Unit, (-), (+), (<), (==), (>=), (/=), show, discard, otherwise, negate)

import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))


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


{-
  Section 5.16 to 5.19
-}

head :: ∀ a. List a -> Maybe a 
head Nil = Nothing
head (x : _) = Just x

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just (xs)

tail' :: ∀ a. List a -> List a
tail' Nil = Nil
tail' (_ : xs) = xs

last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x) = 
  if length (x) == 1 then     
    head x 
  else last (tail' x)


{-
  Section 5.19 to 5.25
-}

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
init l = Just $ go l where
  go (Nil) = Nil
  go (_ : Nil) = Nil
  go (x : xs) = x : go(xs)

uncons :: ∀ a. List a -> Maybe {head:: a, tail:: List a}
uncons Nil = Nothing
uncons (x : xs) = Just {head: x, tail: xs}

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index l i = go l i where
  go :: List a -> Int -> Maybe a
  go Nil _ = Nothing
  go (x: xs) ci
    | ci < 0 = Nothing
    | ci == 0 = Just x
    | otherwise = go xs (ci -1) 

infixl 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex _ Nil = Nothing
findIndex pred l = go 0 l where 
  go :: Int -> List a -> Maybe Int
  go _ Nil = Nothing
  go i (x : xs)
    | pred x = Just i
    | otherwise = go (i + 1) xs

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex _ Nil = Nothing
findLastIndex pred l = go 0 l Nothing where
  go _ Nil lst = lst
  go i (x : xs) lst     
    | pred x = go (i + 1) xs (Just i)
    | otherwise = go (i + 1) xs lst
  


test :: Effect Unit
test = do
  -- log (show (flip const 1 2))
  -- log (show (flip' const 1 2))
  -- log $ show $ flip const 1 2
  -- flip const 1 2 # show # log
  -- log $ show $null Nil
  -- log $ show $null ("xyz" : Nil)
  -- log $ show $ snoc (1 : 2 : Nil) 3
  -- log $ show $ length $ 1 : 2 : 3 : Nil
  -- log $ show $ length'' $ 1 : 2 : 3 : Nil
  -- log $ show $ head (Nil :: List Unit)
  -- log $ show $ head ("abc" : "123" : Nil) 
  -- log $ show $ (tail Nil :: Maybe (List Unit))
  -- log $ show $ tail ("abc" : "123" : "xyz" : Nil) 
  -- log $ show $ (last Nil :: Maybe Unit)
  -- log $ show $ last ("a" : "b" : "c" : Nil)
  -- log $ show $ last ("z" : Nil)
  -- log $ show $ init (Nil :: List Unit)
  -- log $ show $ init (1 : Nil)
  -- log $ show $ init (1 : 2 : Nil)
  -- log $ show $ init (1 : 2 : 3 : Nil)
  -- log $show $uncons (1 : 2 : 3 : Nil)
  -- log $show $uncons (1 : Nil)
  -- log $ show $ index (1 : Nil) 4 -- Nothing
  -- log $ show $ index (1 : 2 : 3 : Nil) 1 -- Just 2
  -- log $ show $ index (1 : 2 : 3 : Nil) (-1) -- Just 2
  -- log $ show $ index (Nil :: List Unit) 0
  -- log $ show $ (1 : 2 : 3 : Nil) !! 1  
  -- log $ show $ findIndex (_ >= 2)  (1 : 2 : 3 : Nil)
  -- log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  -- log $ show $ findIndex (10 /= _) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10)  (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10)  (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10)  (11 : 12 : 13 : Nil)
  