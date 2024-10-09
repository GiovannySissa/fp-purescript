module Ch14 where

import Prelude(Unit, (>>>), (<<<))

import Data.Functor(class Functor)

import Effect (Effect)
import Effect.Console (log)

newtype F1 a = F1(Int -> a)

{-
  Combine functions?
  f :: a -> b given by map definition
  g :: Int -> a

  f <<< g ::  Int -> b 
  g <<< f :: a -> b -> Int -> a --- Compiler error!
-}
instance functorF1 :: Functor F1 where
  map f (F1 g) = F1 (f <<< g)
{-
  f' :: b -> a
  f :: a -> b
  g :: a -> Int

  f <<< g ::  Int -> b --- Compiler error!
  g <<< f :: a -> Int --- Compiler error!
  g <<< f' :: a -> Int --- Compiler error!

  we need a contravariant Functor f'
-}
-- newtype C1 a = C1 (a ->Int)
-- instance functorC1 :: Functor (C1 a) where
--   map f (C1 g) = C1 (f <<< g) We can not create a Functor instance for C1 type

class Contravariant f where
  cmap :: ∀ a b. (b -> a) -> f a -> f b
infix 4 cmap as >$<


newtype C1 a = C1 (a ->Int)
instance contravariantC1 :: Contravariant C1 where
  cmap f (C1 g) = C1 (g <<< f)

newtype F2 a = F2(a -> Int -> Int)

instance contravariantF2:: Contravariant (F2) where
{-
  f :: b -> a
  g :: a -> Int -> Int
  h :: b -> Int -> Int 
-}
  cmap f (F2 g) = F2 ( g <<< f)

newtype F3 a = F3 (Int -> Int -> a)
{-
  f :: b -> a
  g :: Int -> Int -> a
  h :: b -> Int -> Int 
  functions for  F3 contramap instance  are not composable :< 

  f:: a -> b
  g :: Int -> Int -> a ==  g Int ::  Int -> a 
  in that sense we might create a functor instance for F3
-}

instance functorF3 :: Functor F3 where
  map f (F3 g) = F3 \x -> f <<< g x  

newtype F4 a = F4 ((a -> Int) -> Int)
{-
  As F4 has an input we guess we are gonna be able to create a Contravariant
  f :: b -> a
  g :: (a -> Int) -> Int
  g' :: (b -> Int) -> Int after composition, which means  we can't do it  

  a functor implementation neither is possible, we need to map 
  (a -> Int) -> Int to (b -> Int) -> Int

  \h -> ??? ::  (b -> Int) -> Int 
  h :: (b -> Int) this is the 1st parameter to final function
  g :: (a -> Int) -> Int

  \h -> g ??? ::  (b -> Int) -> Int  we can't pass h since it's (b->Int) type and we need (a->Int)
  f :: a -> b

  h <<< f :: a -> Int

  \h -> g (h <<< f) ::  (b -> Int) -> Int

  since f is the type (a -> b) means we should be able to write a functor instance for F4
  but according to the rules we cannot that! since the polymorphic type is the input which means we shoudl
  have a contravariant

-}

instance functorF4 :: Functor F4 where
  map f (F4 g) = F4 \h -> g(h <<< f)


newtype F5 a = F5((Int -> a) -> Int)
{- As rules were defy for the type F4 we gonna try to 
  create a functor even known the polymorphic type is not the output 

  f :: b -> a
  g :: (Int -> a) -> Int
  \h -> g ???  :: (Int -> b) -> Int -- mapped function
  h  :: (Int ->b) -- 1st parameter of mapped fn
  
  f <<< h :: Int -> a 
-}

instance contravariantF5 :: Contravariant F5 where
  cmap f (F5 g) = F5 \h -> g (f <<< h)

test :: Effect Unit
test = do
  log "Placeholder CH14"