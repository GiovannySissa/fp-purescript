module Ch8 where

import Prelude(Unit, discard, ($), show)

import Effect (Effect)

import Effect.Console (log)

import Data.List (List(..), (:))
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))



fromEmptyList :: ∀ a. List a -> Maybe (NonEmptyList a)
fromEmptyList Nil = Nothing
fromEmptyList (x : xs) = Just $ NonEmptyList $ x :| xs

-- class Monoid g <= Group g where
--   ginverse :: g -> g


-- type Abelian a b = Group a => Conmutative a => b -- it has constrains at first glance it might be confused; Conmutative is a typeclass

-- type ConstraintAbelian a b = Abelian a b

 -- example how alias might be used
-- find :: ∀ a. Abelian a (Set a -> Maybe a)
-- -- expanded version
-- find :: ∀ a. Group a => Conmutative a => Set a -> Maybe a


-- class Semiring a where
--   add :: a -> a -> a
--   zero :: a
--   mul :: a -> a -> a
--   one :: a

-- infix 6 add as + 
-- infix 7 mul as * 

-- instance semiringInt :: Semiring Int where
--   add = intAdd
--   zero = 0
--   mul = intMul
--   one = 1

-- foreign import intAdd :: Int -> Int -> Int 
-- foreign import intMul :: Int -> Int -> Int 


test :: Effect Unit
test = do
  log $show $ fromEmptyList (1 : 2 : 2 : 6 : Nil)
  log $show $ fromEmptyList (Nil :: List String )




